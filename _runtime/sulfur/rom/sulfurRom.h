#ifndef SULFUR_ROM_H
#define SULFUR_ROM_H

typedef struct sf_rom {
	tex_array_t* texArr;
		/* loading buffer */
	pthread_mutex_t loadMutex;
	resLoadList_t* resList;
	int32_t compTotal;
} sf_rom_t;

sf_rom_t* initSfRom() {
	sf_rom_t* self = (sf_rom_t*) malloc(sizeof(sf_rom_t));
	self->texArr = NULL;
	pthread_mutex_init(&self->loadMutex, NULL);
	self->resList = newResList();
	self->compTotal = 0;
	return self;
}

void _updateRom(sf_rom_t* rom) {
	// initialize sulfur's texture array if applicable
	if (rom->texArr == NULL) {
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
			//initTexImage(imageData, nextRes->a, nextRes->b, (char*) nextRes->xArgs);
			*nextRes->storePtr = (void*) imageData;
			rom->compTotal = rom->compTotal + 1;
		} else if (nextRes->type == R_SPRITE) {
			int* i_args = nextRes->iArgs;
			tex_image_t* imgPtr = *((tex_image_t**) nextRes->xArgs);
			sprite_t* sprite = initSprite(imgPtr, i_args[0], i_args[1], i_args[2], 1, 1);
			*nextRes->storePtr = (void*) sprite;
			rom->compTotal = rom->compTotal + 1;
		}
	}
	pthread_mutex_unlock(&rom->loadMutex);
}
#endif