#ifndef SPRITE_H
#define SPRITE_H

const int TILE_SIZE = 8;

	/* sprite datatype */

typedef struct sprite {
	tex_image_t* image;
	int tw, th;
	int spanWidth;
	float fx, fy;
	float fw, fh;
} sprite_t;

sprite_t* initSprite(tex_image_t* image, int x, int y, int spanWidth, int tw, int th) {
	sprite_t* sprite = malloc(sizeof(sprite_t));
	sprite->image = image;
	sprite->tw = tw;
	sprite->th = th;
	sprite->spanWidth = spanWidth;
	/*sprite->fx = (float) (x * TILE_SIZE) / image->width;
	sprite->fy = (float) (y * TILE_SIZE) / image->height;
	sprite->fw = (float) tw / (image->width / TILE_SIZE);
	sprite->fh = (float) th / (image->height / TILE_SIZE);*/
	return sprite;
}

#endif