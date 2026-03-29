#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <malloc.h>
#include <pthread.h>

#include "glad.h"
#include "glfw3.h"
#include "exit_log.h"
#include "sulfur_bindings.h"

#include "jas_array.h"
#include "jas_string.h"
#include "os.h"

	/* runtime hooks */

void _none_main();

void* wrap_main(void* arg) {
	_none_main();
	return NULL;
}

	/* bootstrap main loop  */

int main(void) {
	/*
		FULL SULFUR RUNTIME INITIALIZATION
	*/
	int WIDTH = 640;
	int HEIGHT = 480;

	// initialize GLFW
	if (!glfwInit()) {
		exit_log("Could not initialize GLFW.", "");
	}
	glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
	glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
	glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
	glfwWindowHint(GLFW_RESIZABLE, GLFW_FALSE);

	// initialize window
	GLFWwindow* window = glfwCreateWindow(WIDTH, HEIGHT, "Dusk", NULL, NULL);
	if (!window) {
		glfwTerminate();
		exit_log("Could not create GLFW window.", "");
	}
	glfwMakeContextCurrent(window);

	// gl binding loader
	if (!gladLoadGLLoader((GLADloadproc) glfwGetProcAddress)) {
		glfwTerminate();
		exit_log("Loading OpenGL bindings failed.", "");
	}

	// initialize sulfur
	sulfur_t* sulfur_local = initSulfur(WIDTH, HEIGHT);
	sulfur = sulfur_local;

	GLfloat zMat[16] = {
		1.0f, 0.0f, 0.0f, 0.0f,
		0.0f, 1.0f, 0.0f, 0.0f,
		0.0f, 0.0f, 1.0f, 0.0f,
		0.0f, 0.0f, 0.0f, 1.0f
	};
	//glyph_t g1 = { G_BOX, 10, 10, 20, 20 };

	/*
		SULFUR RUNTIME MULTI-THREAD SPLIT
	*/

	pthread_t main_thread;
	pthread_create(&main_thread, NULL, wrap_main, NULL);
	pthread_create(&main_thread, NULL, initRomLoad, sulfur_local);

	// basic render loop
	while (!glfwWindowShouldClose(window)) {
		render(zMat, sulfur_local);
		updateRom(sulfur_local);
		// , zMat, &g1
		glfwSwapBuffers(window);
		glfwPollEvents();
	}

	pthread_detach(main_thread);
	glfwDestroyWindow(window);
	glfwTerminate();

	return 0;
}