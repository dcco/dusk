#ifndef SULFUR_ROM_H
#define SULFUR_ROM_H

typedef struct shared_rom {
		// initialization arguments
	int8_t argInit;
	int32_t arrTotal;
	int32_t arrWidth;
	int32_t arrHeight;
		// actual data
	tex_array_t* texArr;
} shared_rom_t;

	/*
		sulfur rom: abstraction of all the images/audio/etc
			that must be loaded from memory.
	*/

typedef struct sf_rom {
	//tex_array_t* texArr;
		/* loading buffer */
	pthread_mutex_t loadMutex;
	int32_t activeGroup;
	int8_t init;
		/* shared data */
	shared_rom_t shared;
		/* rom groups */
	int32_t groupTotal;
	resGroup_t** groupList;
	//resLoadList_t* resList;
	//int32_t compTotal;
} sf_rom_t;

sf_rom_t* newSfRom() {
	sf_rom_t* self = (sf_rom_t*) malloc(sizeof(sf_rom_t));
	//self->texArr = NULL;
	pthread_mutex_init(&self->loadMutex, NULL);
	self->activeGroup = -1;
	self->init = 0;
	self->shared.argInit = 0;
	return self;
}

void _initSfRom(sf_rom_t* rom, int32_t group_total) {
	pthread_mutex_lock(&rom->loadMutex);
	rom->init = 1;
	rom->groupTotal = group_total;
	rom->groupList = (resGroup_t**) malloc(sizeof(resGroup_t*) * group_total);
	for (int i = 0; i < group_total; i++) {
		rom->groupList[i] = newResGroup();
	}
	pthread_mutex_unlock(&rom->loadMutex);
	//self->resList = newResList();
	//self->compTotal = 0;
}

	/*
		rom "update": incrementally processes resources in queue (based on active group)
	*/

void _procResItem(shared_rom_t* shared, resLoadItem_t* item) {
	if (item->type == R_IMAGE) {
		tex_image_t* imageData = (tex_image_t*) malloc(sizeof(tex_image_t));
		initTexImage(shared->texArr, imageData, item->storeId + 1, (char*) item->xArgs);
		// - does not free so the raw data may be accessed by user
		// stbi_image_free(item->xArgs);
		*item->storePtr = (void*) imageData;
	} else if (item->type == R_FONT) {
		if (item->iArgs == NULL) return;
		fontFamily_t* fontData = newFontFamily(shared->texArr, item->storeId + 1, item->iArgs, (char*) item->xArgs);
		*item->storePtr = (void*) fontData;
	} else if (item->type == R_SPRITE) {
		int* i_args = item->iArgs;
		void** x_args = (void**) item->xArgs;
		tex_image_t* imgPtr = *((tex_image_t**) x_args[0]);
		sprite_t* sprite = initSprite(imgPtr, i_args[0], i_args[1], i_args[2], i_args[3], i_args[4]);
		*item->storePtr = (void*) sprite;
	}
}

void _updateRom(sf_rom_t* rom) {
	pthread_mutex_lock(&rom->loadMutex);
	if (!rom->init || !rom->shared.argInit) {
		pthread_mutex_unlock(&rom->loadMutex);
		return;
	}
	// initialize shared data first (texture atlas, etc)
	shared_rom_t* shared = &rom->shared;
	if (shared->texArr == NULL) {
		shared->texArr = initTexArray(shared->arrTotal, shared->arrWidth, shared->arrHeight);
	}
	// get active loading group
	resGroup_t* curGroup = NULL;
	int32_t activeGroup = rom->activeGroup;
	if (activeGroup == -1) {
		pthread_mutex_unlock(&rom->loadMutex);
		return;
	}
	curGroup = rom->groupList[activeGroup];
	pthread_mutex_unlock(&rom->loadMutex);
	if (curGroup == NULL) return;
	// load resources in group
	pthread_mutex_lock(&rom->loadMutex);
	resLoadItem_t* nextRes = takeResList(curGroup->loadList);
	if (nextRes != NULL) {
		_procResItem(shared, nextRes);
		if (curGroup->finalFlag && emptyResList(curGroup->loadList)) {
			rom->activeGroup = -1;
		}
	}
	pthread_mutex_unlock(&rom->loadMutex);

	// initialize sulfur's texture array if applicable
	/*if (rom->texArr == NULL) {
		pthread_mutex_lock(&rom->loadMutex);
		if (rom->resList->meta.init) {
			resListMeta_t* meta = &rom->resList->meta;
			rom->texArr = initTexArray(meta->total, meta->width, meta->height);
		}
		pthread_mutex_unlock(&rom->loadMutex);
	}
	// load remaining resources
	pthread_mutex_lock(&rom->loadMutex);
	resLoadItem_t* nextRes = takeResList(rom->resList);
	if (nextRes != NULL) {
		if (nextRes->type == R_IMAGE) {
			tex_image_t* imageData = (tex_image_t*) malloc(sizeof(tex_image_t));
			initTexImage(rom->texArr, imageData, nextRes->storeId + 1, (char*) nextRes->xArgs);
			// - does not free so the raw data may be accessed by user
			// stbi_image_free(nextRes->xArgs);
			*nextRes->storePtr = (void*) imageData;
			rom->compTotal = rom->compTotal + 1;
		} else if (nextRes->type == R_SPRITE) {
			int* i_args = nextRes->iArgs;
			tex_image_t* imgPtr = *((tex_image_t**) nextRes->xArgs);
			sprite_t* sprite = initSprite(imgPtr, i_args[0], i_args[1], i_args[2], i_args[3], i_args[4]);
			*nextRes->storePtr = (void*) sprite;
			rom->compTotal = rom->compTotal + 1;
		}
	}
	pthread_mutex_unlock(&rom->loadMutex);*/
}
#endif