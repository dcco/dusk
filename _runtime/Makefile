
SULFUR_DIRS := $(shell find ./sulfur -type d)
SULFUR_INCLUDES = $(foreach dir,$(SULFUR_DIRS),-I$(dir))
SULFUR_LINKS = $(foreach dir,$(SULFUR_DIRS),-I$(dir))

IFLAGS = -I./dep -I./sys $(SULFUR_INCLUDES)
LDFLAGS = -L./dep -L./sys $(SULFUR_LINKS)

runtime.exe: FORCE
	x86_64-w64-mingw32-gcc -o runtime.exe dep/glad.c runtime.c $(IFLAGS) $(LDFLAGS) -pthread -lopengl32 -lglfw3dll

main.exe: FORCE
	x86_64-w64-mingw32-gcc -o main.exe dep/glad.c runtime.c j_out.o -DAPP $(IFLAGS) $(LDFLAGS) -pthread -lopengl32 -lglfw3dll

main: FORCE
	gcc -o main dep/glad.c runtime.c j_out.o $(IFLAGS) $(LDFLAGS) -pthread -lopengl32 -lglfw3dll

FORCE: ;