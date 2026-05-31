
DUSK_FLAGS =

SULFUR_DIRS := $(shell find ./sulfur -type d)
SULFUR_INCLUDES = $(foreach dir,$(SULFUR_DIRS),-I$(dir))
SULFUR_LINKS = $(foreach dir,$(SULFUR_DIRS),-I$(dir))

IFLAGS = -I./dep -I./sys $(SULFUR_INCLUDES)
LDFLAGS = -L./dep -L./sys $(SULFUR_LINKS)

runtime.exe: FORCE
	x86_64-w64-mingw32-gcc -o runtime.exe dep/glad.c runtime.c $(DUSK_FLAGS) $(IFLAGS) $(LDFLAGS) -pthread -lopengl32 -lglfw3dll -lgdi32

main.exe: FORCE
	x86_64-w64-mingw32-gcc -o main.exe dep/glad.c runtime.c j_out.o -DAPP $(DUSK_FLAGS) $(IFLAGS) $(LDFLAGS) -pthread -lopengl32 -lglfw3dll -lgdi32

main: FORCE
	gcc -o main dep/glad.c runtime.c j_out.o $(DUSK_FLAGS) $(IFLAGS) $(LDFLAGS) -pthread -lopengl32 -lglfw3dll -lgdi32

FORCE: ;