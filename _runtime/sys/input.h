#ifndef INPUT_H
#define INPUT_H

#define KEY_TOTAL (GLFW_KEY_LAST + 1)

typedef struct kbm_input {
	GLFWwindow* window;
	int8_t down[KEY_TOTAL];
	int8_t press[KEY_TOTAL];
	int8_t mbDown[2];
	int8_t mbPress[2];
} kbm_input_t;

kbm_input_t* initInput(GLFWwindow* window) {
	kbm_input_t* input = (kbm_input_t*) malloc(sizeof(kbm_input_t));
	input->window = window;
	for (int i = 0; i < KEY_TOTAL; i++) {
		input->down[i] = 0;
		input->press[i] = 0;
	}
	for (int i = 0; i < 2; i++) {
		input->mbDown[i] = 0;
		input->mbPress[i] = 0;
	}
	return input;
}

void updateInput(kbm_input_t* input) {
	for (int i = 0; i < KEY_TOTAL; i++) {
		int8_t cur = glfwGetKey(input->window, i) == GLFW_PRESS;
		input->press[i] = cur && !input->down[i];
		input->down[i] = cur;
	}
	for (int i = 0; i < 2; i++) {
		int b = GLFW_MOUSE_BUTTON_LEFT;
		if (i == 1) b = GLFW_MOUSE_BUTTON_RIGHT;
		int8_t cur = glfwGetMouseButton(input->window, b) == GLFW_PRESS;
		input->mbPress[i] = cur && !input->mbDown[i];
		input->mbDown[i] = cur;
	}
}

	/*
		external runtime bindings
	*/

const int32_t K_left = GLFW_KEY_LEFT;
const int32_t K_right = GLFW_KEY_RIGHT;
const int32_t K_up = GLFW_KEY_UP;
const int32_t K_down = GLFW_KEY_DOWN;
const int32_t K_space = GLFW_KEY_SPACE;
const int32_t K_z = GLFW_KEY_Z;
const int32_t K_x = GLFW_KEY_X;
const int32_t K_c = GLFW_KEY_C;
const int32_t K_w = GLFW_KEY_W;
const int32_t K_a = GLFW_KEY_A;
const int32_t K_s = GLFW_KEY_S;
const int32_t K_d = GLFW_KEY_D;
const int32_t K_q = GLFW_KEY_Q;
const int32_t K_e = GLFW_KEY_E;
const int32_t K_n = GLFW_KEY_N;
const int32_t K_m = GLFW_KEY_M;

kbm_input_t* mainInput = NULL;

extern void _none_Sys_Input_inUpdate() {
	updateInput(mainInput);
}

extern int8_t _Key_Sys_Input_keyDown(int32_t c) {
	return mainInput->down[c];
}

extern int8_t _Key_Sys_Input_keyPress(int32_t c) {
	return mainInput->press[c];
}

extern int8_t _Key_Sys_Input_mouseDown(int32_t c) {
	if (c == K_right) return mainInput->mbDown[1];
	else return mainInput->mbDown[0];
}

extern int8_t _Key_Sys_Input_mousePress(int32_t c) {
	if (c == K_right) return mainInput->mbPress[1];
	else return mainInput->mbPress[0];
}

typedef struct mpos {
	int32_t x;
	int32_t y;
} mpos_t;

extern void _none_Sys_Input_mousePos(mpos_t* ret) {
	double fx, fy;
	glfwGetCursorPos(mainInput->window, &fx, &fy);
	ret->x = (int32_t) fx;
	ret->y = (int32_t) fy;
}

#endif