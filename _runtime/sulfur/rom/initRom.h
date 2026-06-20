#ifndef LOAD_ROM_H
#define LOAD_ROM_H

	/*
		external resource datatypes
	*/

typedef struct gen_res_list {
	int32_t total;
	void* data;
} gen_res_list_t;

typedef struct simp_res {
	R_LOAD_TYPE type;
	int32_t groupId;
	char* url;
	int32_t* iArgs;
	void** storePtr;
} simp_res_t;

typedef struct comp_res {
	R_LOAD_TYPE type;
	int32_t groupId;
	int32_t* iArgs;
	void** xArgs;
	void** storePtr;
} comp_res_t;

char* readFile(char* url, size_t* size) {
	FILE *f = fopen(url, "rb");
	if (!f) return NULL;
	// read file size
	if (fseek(f, 0, SEEK_END) != 0) {
		fclose(f); return NULL;
	}
	*size = ftell(f);
	if (*size < 0) {
		fclose(f); return NULL;
	}
	fseek(f, 0, SEEK_SET);
	// read file data
	char* data = malloc(sizeof(char) * (*size));
	size_t bytesRead = fread(data, 1, *size, f);
	fclose(f);
	if (bytesRead != *size) {
		free(data); return NULL;
	}
	return data;
}

void loadSimpRes(int i, resLoadItem_t* item, char* full_url, simp_res_t* res) {
	int n;
	item->type = res->type;
	item->storePtr = res->storePtr;
	item->storeId = i;
	item->iArgs = res->iArgs;
	// type specific loading
	if (res->type == RC_IMAGE) {
		// -- always load 4 channels
		char* img = stbi_load(full_url, &item->a, &item->b, &n, 4);
		if (img == NULL) {
			// TODO: throw proper logging exception
			printf("Failed to load: %s\n", full_url);
			return;
		}
		item->xArgs = img;
	} else if (res->type == RC_FONT) {
		size_t font_size;
		char* ttfData = readFile(full_url, &font_size);
		if (ttfData == NULL) {
			// TODO: throw proper logging exception
			printf("Failed to load: %s\n", full_url);
			return;
		}
		item->xArgs = ttfData;
	} else {
		exit_log("BUG?: Unknown resource type encountered while initializing ROM.", "");
	}
}

	/*
		simple dynamic string datatype (for string operations)
	*/

typedef struct dyn_string {
	size_t cap;
	char* data;
} dyn_string_t;

dyn_string_t* newDynString(size_t cap) {
	dyn_string_t* s = (dyn_string_t*) malloc(sizeof(dyn_string_t));
	s->cap = cap;
	s->data = (char*) malloc(sizeof(char) * cap);
	return s;
}

void rawCatDynString(dyn_string_t* s, char* c1, char* c2) {
	// increase memory size if required
	size_t new_cap = strlen(c1) + strlen(c2);
	if (s->cap < new_cap) {
		s->cap = new_cap * 2;
		s->data = realloc(s->data, s->cap + 1);
	}
	strcpy(s->data, c1);
	strcat(s->data, c2);
}

void freeDynString(dyn_string_t* s) {
	free(s->data);
	free(s);
}

	/* resource loading */

extern char* rom_dir;

extern gen_res_list_t res_list;
extern gen_res_list_t comp_res_list;
extern gen_res_list_t res_group_names;

/*
extern void* res_url_list[];
extern void* res_ptr_list[];
extern int res_total;

extern void* comp_res_arg_list[];
extern void* comp_res_ptr_list[];
extern int comp_res_total;*/

void* initRomLoad(void* arg) {
	// unpack sulfur rom
	sf_rom_t* rom = ((sulfur_t*) arg)->rom;
	_initSfRom(rom, res_group_names.total);
	// initialize memory to store URL
	dyn_string_t* full_url = newDynString(strlen(rom_dir) * 2);
	/*size_t rom_len = strlen(rom_dir);
	size_t full_len = rom_len * 2;
	char *full_url = malloc(full_len + 1);*/
	// resource data storage
	resLoadItem_t iData;
	// meta data (image count for texture atlas, etc)
	int32_t imageCount = 0;
	int32_t w = 0;
	int32_t h = 0;
	// iterate through each URL
	simp_res_t* res_list_data = (simp_res_t*) res_list.data;
	for (int i = 0; i < res_list.total; i++) {
		// read URL name
		rawCatDynString(full_url, rom_dir, res_list_data[i].url);
		// increase memory size if required
		/*size_t cur_len = rom_len + strlen(res_url_list[i]);
		if (full_len < cur_len) {
			full_len = cur_len * 2;
			full_url = realloc(full_url, full_len + 1);
		}
		// read URL name
		strcpy(full_url, rom_dir);
		strcat(full_url, res_url_list[i]);*/
		// load simple resource
		loadSimpRes(imageCount, &iData, full_url->data, &res_list_data[i]);
		// load image data
		/*iData.type = R_IMAGE;
		iData.storePtr = res_ptr_list[i];
		iData.storeId = i;
		char* img = stbi_load(full_url, &iData.a, &iData.b, &n, 4);
		if (img == NULL) {
			// TODO: throw exception
			printf("Failed to load: %s\n", full_url);
			return NULL;
		}
		iData.xArgs = img;
		// check image size
		if (i == 0) {
			w = iData.a;
			h = iData.b;
			// pass meta-information along
			pthread_mutex_lock(&rom->loadMutex);
			resListMeta_t* meta = &rom->resList->meta;
			meta->init = 1;
			meta->total = res_total;
			meta->width = w;
			meta->height = h;
			pthread_mutex_unlock(&rom->loadMutex);
		} else {
			if (iData.a != w || iData.b != h) {
				exit_log("Inconsistent texture sizes for texture atlas.", "");
			}
		}*/
		// meta data
		if (iData.type == R_IMAGE) {
			imageCount = imageCount + 1;
			if (iData.a > w) w = iData.a;
			if (iData.b > h) h = iData.b;
		} else if (iData.type == R_FONT) {
			if (iData.iArgs != NULL) imageCount = imageCount + iData.iArgs[0];
		}
		// pass image data to sulfur's resource loader
		pthread_mutex_lock(&rom->loadMutex);
		int32_t groupId = res_list_data[i].groupId;
		addResList(rom->groupList[groupId]->loadList, &iData);
		pthread_mutex_unlock(&rom->loadMutex);
	}
	free(full_url);
	// iterate through each composite resource
	for (int i = 0; i < comp_res_list.total; i++) {
		comp_res_t* comp_res = &((comp_res_t*) comp_res_list.data)[i];
		// initialize composite resource data
		iData.type = comp_res->type;
		iData.storePtr = comp_res->storePtr;
		iData.iArgs = comp_res->iArgs;
		iData.xArgs = comp_res->xArgs;
		// pass sprite data to sulfur's resource loader
		pthread_mutex_lock(&rom->loadMutex);
		int32_t groupId = comp_res->groupId;
		addResList(rom->groupList[groupId]->loadList, &iData);
		pthread_mutex_unlock(&rom->loadMutex);
	}
	/*for (int i = 0; i < comp_res_total; i++) {
		// read arguments
		void** comp_res_args = comp_res_arg_list[i];
		// sprite case (only case atm)
		iData.type = R_SPRITE;
		iData.storePtr = comp_res_ptr_list[i];
		iData.iArgs = (int*) comp_res_args[0];
		iData.xArgs = (void**) comp_res_args[1];
		// pass sprite data to sulfur's resource loader
		pthread_mutex_lock(&rom->loadMutex);
		addResList(rom->resList, &iData);
		pthread_mutex_unlock(&rom->loadMutex);
	}*/
	pthread_mutex_lock(&rom->loadMutex);
	// initialize shared resource metadata
	shared_rom_t* shared = &rom->shared;
	shared->argInit = 1;
	shared->arrTotal = imageCount;
	if (imageCount != 0) {
		shared->arrWidth = w;
		shared->arrHeight = h;
	} else {
		// default meta-information if no ROM
		shared->arrWidth = 256;
		shared->arrHeight = 256;
	}
	// finalize groups
	for (int i = 0; i < res_group_names.total; i++) {
		rom->groupList[i]->finalFlag = 1;
	}
	pthread_mutex_unlock(&rom->loadMutex);
	return NULL;
}

void waitRom(sf_rom_t* rom) {
	// TODO: have it wait on specific groups rather than all groups
	// read group total
	int8_t total;
	pthread_mutex_lock(&rom->loadMutex);
	total = rom->groupTotal;
	pthread_mutex_unlock(&rom->loadMutex);
	for (int i = 0; i < total; i++) {
		// assign next active group
		pthread_mutex_lock(&rom->loadMutex);
		rom->activeGroup = i;
		pthread_mutex_unlock(&rom->loadMutex);
		// loop until everything is finished loading
		int8_t done = 0;
		while (!done) {
			sleep_ms(30);
			pthread_mutex_lock(&rom->loadMutex);
			if (rom->activeGroup == -1) done = 1; 
			pthread_mutex_unlock(&rom->loadMutex);
		}
	}
	/*int32_t mainTotal = res_total + comp_res_total;
	int32_t loadTotal;
	pthread_mutex_lock(&rom->loadMutex);
	loadTotal = rom->compTotal;
	pthread_mutex_unlock(&rom->loadMutex);
	while (loadTotal != mainTotal) {
		sleep_ms(30);
		pthread_mutex_lock(&rom->loadMutex);
		loadTotal = rom->compTotal;
		pthread_mutex_unlock(&rom->loadMutex);
	}*/
}


#endif