#ifndef INPUT_H
#define INPUT_H

#define KEY_TOTAL (GLFW_KEY_LAST + 1)

typedef struct kbm_input {
	GLFWwindow* window;
	int8_t down[KEY_TOTAL];
	int8_t press[KEY_TOTAL];
} kbm_input_t;

kbm_input_t* initInput(GLFWwindow* window) {
	kbm_input_t* input = (kbm_input_t*) malloc(sizeof(kbm_input_t));
	input->window = window;
	for (int i = 0; i < KEY_TOTAL; i++) {
		input->down[i] = 0;
		input->press[i] = 0;
	}
	return input;
}

void updateInput(kbm_input_t* input) {
	for (int i = 0; i < KEY_TOTAL; i++) {
		int8_t cur = glfwGetKey(input->window, i) == GLFW_PRESS;
		input->press[i] = cur && !input->down[i];
		input->down[i] = cur;
	}
}

	/*
		external runtime bindings
	*/

const int32_t K_left = GLFW_KEY_LEFT;
const int32_t K_right = GLFW_KEY_RIGHT;
const int32_t K_up = GLFW_KEY_UP;
const int32_t K_down = GLFW_KEY_DOWN;
const int32_t K_z = GLFW_KEY_Z;
const int32_t K_x = GLFW_KEY_X;

kbm_input_t* mainInput = NULL;

extern void _none_Sys_Input_update() {
	updateInput(mainInput);
}

extern int8_t _Key_Sys_Input_keyDown(int32_t c) {
	return mainInput->down[c];
}

extern int8_t _Key_Sys_Input_keyPress(int32_t c) {
	return mainInput->press[c];
}

#endif