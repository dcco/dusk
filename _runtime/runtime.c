#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <math.h>
#include <string.h>
#include <malloc.h>
#include <pthread.h>

#ifndef M_PI
#define M_PI 3.1415926536f
#endif

#include "glad.h"
#include "glfw3.h"

#include "time_api.h"
#include "exit_log.h"
#include "gc.h"
#include "dusk_string.h"

#include "sulfur_bindings.h"
#include "input.h"
#include "xoshiro.h"
#include "os.h"

	/* runtime hooks */

void _none_main();

void* wrap_main(void* arg) {
	srand(time(NULL) ^ getpid());
	_none_main();
	return NULL;
}

	/* bootstrap main loop  */

int main(void) {
	/*
		FULL SULFUR RUNTIME INITIALIZATION
	*/
	int WIDTH = 960;
	int HEIGHT = 640;
	//int WIDTH = 640;
	//int HEIGHT = 480;
	gc_init();

	// initialize GLFW
	if (!glfwInit()) {
		exit_log("Could not initialize GLFW.", "");
	}
	glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
	glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
	glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_ANY_PROFILE);
	glfwWindowHint(GLFW_RESIZABLE, GLFW_FALSE);

	glfwWindowHint(GLFW_DEPTH_BITS, 24);
	glfwWindowHint(GLFW_DOUBLEBUFFER, GLFW_TRUE);

	// initialize window
	GLFWwindow* window = glfwCreateWindow(WIDTH, HEIGHT, "Dusk", NULL, NULL);
	if (!window) {
		glfwTerminate();
		exit_log("Could not create GLFW window.", "");
	}
	glfwMakeContextCurrent(window);
	glfwSwapInterval(1);

	// gl binding loader
	if (!gladLoadGLLoader((GLADloadproc) glfwGetProcAddress)) {
		glfwTerminate();
		exit_log("Loading OpenGL bindings failed.", "");
	}

	// check open gl version
	if (glGetString(GL_VERSION)) {
		printf("OpenGL Version: %s\n", glGetString(GL_VERSION));
	} else {
		exit_log("OpenGL context failed!", "");
	}

	// depth bits test
	/*GLint depthBits = 0;
	glGetIntegerv(GL_DEPTH_BITS, &depthBits);
	printf("Depth bits: %d\n", depthBits);*/

	// initialize input
	kbm_input_t* input_local = initInput(window);
	mainInput = input_local;

	// initialize sulfur
	sulfur_t* sulfur_local = initSulfur(WIDTH, HEIGHT);
	sulfur = sulfur_local;

	/*
		SULFUR RUNTIME MULTI-THREAD SPLIT
	*/

	pthread_t main_thread;
	pthread_create(&main_thread, NULL, wrap_main, NULL);
	pthread_create(&main_thread, NULL, initRomLoad, sulfur_local);
	
	// basic render loop
	while (!glfwWindowShouldClose(window)) {
		updateRom(sulfur_local);

		if (render(sulfur_local)) glfwSwapBuffers(window);
		glfwPollEvents();
	}

	pthread_detach(main_thread);
	glfwDestroyWindow(window);
	glfwTerminate();

	return 0;
}