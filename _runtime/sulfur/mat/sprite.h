#ifndef SPRITE_H
#define SPRITE_H

const int TILE_SIZE = 8;
const uint8_t SP_OFF_R = 127;
const uint8_t SP_OFF_G = 201;
const uint8_t SP_OFF_B = 255;

	/*
		sprite datatype:
		- (rw, rh): frame size (in tiles)
		- (tw, th): frame size (in pixels)
		- spanWidth: frames per row
		- (fx, fy): spritesheet position [0.0, 1.0]
		- (fw, fh): frame size [0.0, 1.0]
		- (offX, offY): frame offset (in draw units)
	*/

typedef struct sprite {
	tex_image_t* image;
	int rw, rh;
	int tw, th;
	int spanWidth;
	float fx, fy;
	float fw, fh;
	float offX, offY;
} sprite_t;

sprite_t* initSprite(tex_image_t* image, int x, int y, int spanWidth, int tw, int th) {
	sprite_t* sprite = malloc(sizeof(sprite_t));
	sprite->image = image;
	sprite->rw = tw;
	sprite->rh = th;
	sprite->tw = tw * TILE_SIZE;
	sprite->th = th * TILE_SIZE;
	sprite->spanWidth = spanWidth;
	sprite->fx = (float) (x * TILE_SIZE) / image->width;
	sprite->fy = (float) (y * TILE_SIZE) / image->height;
	sprite->fw = (float) sprite->tw / image->width;
	sprite->fh = (float) sprite->th / image->height;
	sprite->offX = 0.0;
	sprite->offY = 0.0;
	for (int i = 0; i < sprite->tw; i++) {
		for (int j = 0; j < sprite->th; j++) {
			int px = (x * TILE_SIZE) + i;
			int py = (y * TILE_SIZE) + j;
			if (checkPixel(image, px, py, SP_OFF_R, SP_OFF_G, SP_OFF_B)) {
				sprite->offX = (float) i / TILE_SIZE;
				sprite->offY = (float) j / TILE_SIZE;
				return sprite;
			}
		}
	}
	return sprite;
}

#endif