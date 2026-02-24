
runtime.exe: FORCE
	x86_64-w64-mingw32-gcc -o runtime.exe dep/glad.c runtime.c -I./dep -I./sys -I./sulfur -L./dep -L./sys -L./sulfur -pthread -lopengl32 -lglfw3dll

main.exe: FORCE
	x86_64-w64-mingw32-gcc -o main.exe dep/glad.c runtime.c j_out.o -DAPP -I./dep -I./sys -I./sulfur -L./dep -L./sys -L./sulfur -pthread  -lopengl32 -lglfw3dll

main: FORCE
	gcc -o main dep/glad.c runtime.c j_out.o -I./dep -I./sys -I./sulfur -L./dep -L./sys -L./sulfur -pthread -lopengl32 -lglfw3dll

FORCE: ;