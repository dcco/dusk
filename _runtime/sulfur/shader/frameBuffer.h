#ifndef FRAME_BUFFER_H
#define FRAME_BUFFER_H

typedef int8_t FBO_LAYER_TYPE;
enum { FBO_COLOR, FBO_DEPTH, FBO_RENDER };

extern const int8_t C_FBO_COLOR;
extern const int8_t C_FBO_DEPTH;
extern const int8_t C_FBO_RENDER;

const int8_t C_FBO_COLOR = FBO_COLOR;
const int8_t C_FBO_DEPTH = FBO_DEPTH;
const int8_t C_FBO_RENDER = FBO_RENDER;

	/*
		buffer layer def: defines a layer of the frame buffer
			(it's purpose / how to create it)
	*/

typedef struct fbo_layer_def {
	FBO_LAYER_TYPE type;
} fbo_layer_def_t;

GLuint newColorBuffer(int32_t w, int32_t h, int32_t i)
{
	GLuint texId;
	glGenTextures(1, &texId);
	if (texId == 0) exit_log("Could not generate color buffer texture.", "");
	GLint maxAttachments;
	glGetIntegerv(GL_MAX_COLOR_ATTACHMENTS, &maxAttachments);
	if (i >= maxAttachments) exit_log("Exceeded supported number of color attachments.", "");

	glBindTexture(GL_TEXTURE_2D, texId);
	glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA16F, w, h, 0, GL_RGBA, GL_FLOAT, NULL);

	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);

	glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0 + i, GL_TEXTURE_2D, texId, 0);
	return texId;
}

GLuint newDepthBuffer(int32_t w, int32_t h)
{
	GLuint texId;
	glGenTextures(1, &texId);
	if (texId == 0) exit_log("Could not generate depth buffer texture.", "");

	glBindTexture(GL_TEXTURE_2D, texId);
	glTexImage2D(
		GL_TEXTURE_2D, 0, GL_DEPTH_COMPONENT32F, w, h, 0,
		GL_DEPTH_COMPONENT, GL_FLOAT, NULL
	);

	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);

	glFramebufferTexture2D(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_TEXTURE_2D, texId, 0);
	return texId;
}

	/*
		frame buffer:
			stores the frame buffer with its associated shader + frame buffer layers
	*/

typedef struct frameBuffer {
	shader_t* shader;
	int32_t width;
	int32_t height;
	GLuint fbo;
	GLuint rbo;
	GLuint* texList;
} frameBuffer_t;

frameBuffer_t* newFrameBuffer(shader_t* shader, int32_t w, int32_t h, int32_t layerTotal, fbo_layer_def_t* layerDefs)
{
	// create initial frame buffer memory
	frameBuffer_t* buffer = (frameBuffer_t*) malloc(sizeof(frameBuffer_t));
	buffer->shader = shader;
	buffer->width = w;
	buffer->height = h;

	// create fbo object
	glUseProgram(shader->prog);
	glGenFramebuffers(1, &buffer->fbo);
	if (buffer->fbo == 0) exit_log("frameBuffer.h - Could not create frame buffer.", "");
	glBindFramebuffer(GL_FRAMEBUFFER, buffer->fbo);

	// initialize buffer layers
	buffer->texList = malloc(sizeof(GLuint) * layerTotal);
	int8_t renderFlag = 0;
	int32_t colorTotal = 0;
	int8_t badTotal = 0;
	for (int i = 0; i < layerTotal; i++) {
		if (layerDefs[i].type == FBO_COLOR) {
			buffer->texList[i] = newColorBuffer(w, h, colorTotal);
			colorTotal = colorTotal + 1;
		} else if (layerDefs[i].type == FBO_DEPTH) {
			buffer->texList[i] = newDepthBuffer(w, h);
		} else if (layerDefs[i].type == FBO_RENDER) {
			renderFlag = 1;
		} else {
			badTotal = badTotal + 1;
		}
	}
	if (layerTotal - badTotal == 0) exit_log("frameBuffer.h - No frame buffer layers.", "");

	// attach color buffers
	if (colorTotal > 0) {
		glBindFramebuffer(GL_FRAMEBUFFER, buffer->fbo);
		GLenum* colorArray = malloc(sizeof(GLenum) * colorTotal);
		for (int i = 0; i < colorTotal; i++) {
			colorArray[i] = GL_COLOR_ATTACHMENT0 + i;
		}
		glDrawBuffers(colorTotal, colorArray);
		free(colorArray);
	} else {
		glDrawBuffer(GL_NONE);
		glReadBuffer(GL_NONE);
	}

	// create + attach render buffer if applicable
	buffer->rbo = 0;
	if (renderFlag) {
		glGenRenderbuffers(1, &buffer->rbo);
		if (buffer->rbo == 0) exit_log("frameBuffer.h - Could not create render buffer.", "");
		glBindRenderbuffer(GL_RENDERBUFFER, buffer->rbo);
		glRenderbufferStorage(GL_RENDERBUFFER, GL_DEPTH24_STENCIL8, w, h);
		glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_STENCIL_ATTACHMENT, GL_RENDERBUFFER, buffer->rbo);
	}

	// final check + return
	GLenum status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
	if (status != GL_FRAMEBUFFER_COMPLETE) {
		//exit_log("frameBuffer.h - Frame buffer incomplete.", "");
		switch (status) {
			case GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT:
				exit_log("frameBuffer.h - Frame buffer incomplete attachments.", "");
			case GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT:
				exit_log("frameBuffer.h - Frame buffer incomplete, missing attachment.", "");
			case GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER:
				exit_log("frameBuffer.h - Frame buffer incomplete draw buffer.", "");
			case GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER:
				exit_log("frameBuffer.h - Frame buffer incomplete read buffer.", "");
			case GL_FRAMEBUFFER_UNSUPPORTED:
				exit_log("frameBuffer.h - Frame buffer incomplete (unsupported).", "");
			default:
				exit_log("frameBuffer.h - Frame buffer incomplete (unknown error).", "");
		}
	}
	glBindFramebuffer(GL_FRAMEBUFFER, 0);
	return buffer;
}

#endif