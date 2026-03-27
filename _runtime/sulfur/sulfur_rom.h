#ifndef SULFUR_ROM_H
#define SULFUR_ROM_H

#define STB_IMAGE_IMPLEMENTATION
#include "stb_image.h"

	/* resource loading */

const char* ROM_DIR = "workspace/game/rom/";

extern void* res_url_list[];
extern void* res_ptr_list[];
extern int res_total;

extern void* comp_res_arg_list[];
extern void* comp_res_ptr_list[];
extern int comp_res_total;

void* load_res(void* arg) {
	// unpack sulfur
	sulfur_t* sulfur = (sulfur_t*) arg;
	// initialize memory to store URL
	size_t rom_len = strlen(ROM_DIR);
	size_t full_len = rom_len * 2;
	char *full_url = malloc(full_len + 1);
	// image data storage
	resLoadItem_t iData;
	int n, w, h;
	// iterate through each URL
	for (int i = 0; i < res_total; i++) {
		// increase memory size if required
		size_t cur_len = rom_len + strlen(res_url_list[i]);
		if (full_len < cur_len) {
			full_len = cur_len * 2;
			full_url = realloc(full_url, full_len + 1);
		}
		// read URL name
		strcpy(full_url, ROM_DIR);
		strcat(full_url, res_url_list[i]);
		// load image data
		iData.type = R_IMAGE;
		iData.storePtr = res_ptr_list[i];
		iData.storeId = i;
		char* img = stbi_load(full_url, &iData.a, &iData.b, &n, 0);
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
			pthread_mutex_lock(&sulfur->loadMutex);
			resListMeta_t* meta = &sulfur->res_list->meta;
			meta->init = 1;
			meta->total = res_total;
			meta->width = w;
			meta->height = h;
			pthread_mutex_unlock(&sulfur->loadMutex);
		} else {
			if (iData.a != w || iData.b != h) {
				exit_log("Inconsistent texture sizes for texture atlas.", "");
			}
		}
		// pass image data to sulfur's resource loader
		pthread_mutex_lock(&sulfur->loadMutex);
		addResList(sulfur->res_list, &iData);
		pthread_mutex_unlock(&sulfur->loadMutex);
		/*printf("Successfully loaded: %s\n", full_url);
		int ix = 0;
		for (int k = 0; k < y; k++) {
			for (int j = 0; j < x; j++) {
				printf("%d, ", img[ix]);
				ix = ix + 1;
			}
			printf("\n");
		}*/
		// int* p = *((int**) res_ptr_list[i]);
	}
	free(full_url);
	// iterate through each composite resource
	for (int i = 0; i < comp_res_total; i++) {
		// read arguments
		void** comp_res_args = comp_res_arg_list[i];
		// sprite case (only case atm)
		iData.type = R_SPRITE;
		iData.storePtr = comp_res_ptr_list[i];
		iData.iArgs = (int*) comp_res_args[0];
		iData.xArgs = (void**) comp_res_args[1];
		// pass sprite data to sulfur's resource loader
		pthread_mutex_lock(&sulfur->loadMutex);
		addResList(sulfur->res_list, &iData);
		pthread_mutex_unlock(&sulfur->loadMutex);
	}
	return NULL;
}

#endif