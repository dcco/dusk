#ifndef RES_GROUP_H
#define RES_GROUP_H

/*
typedef struct resMeta {
	int32_t total;
	int32_t width;
	int32_t height;
} resMeta_t;

/*
	resListMeta_t meta;

	rl->meta.init = 0;
/*
*/

	/*
		resource group: contains a list of resources being loaded/processed
			+ associated metadata / materials
		- loadList: the list of resources to be loaded/processed
		- finalFlag: flag indicating no more resources will be added to queue 
	*/

typedef struct resGroup {
	resLoadList_t* loadList;
	int8_t finalFlag;
} resGroup_t;

resGroup_t* newResGroup() {
	resGroup_t* group = (resGroup_t*) malloc(sizeof(resGroup_t));
	group->loadList = newResList();
	group->finalFlag = 0;
	return group;
}

#endif
