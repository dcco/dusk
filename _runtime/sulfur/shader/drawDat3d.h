
	/*
		base 3d draw data:
			defines the beginning of an object to be sent to the 3d shader
			~ these attributes are always mandatory,
				may be followed by extra attributes defined in the 3d shaders
	*/

typedef struct draw_dat3d {
	float aPos[3];
	float aTexId;
	float aTexUVPos[2];
	float aTexUVSize[2];
} draw_dat3d_t;

const struct shader_attr_def BASE3_ATTR_LIST[6] = {
	{ 1, 3, GL_FLOAT, (void*) offsetof(vertex_t, pos) },
	{ 3, 2, GL_FLOAT, (void*) offsetof(vertex_t, uv) },
	{ 0, 3, GL_FLOAT, (void*) offsetof(draw_dat3d_t, aPos) },
	{ 0, 1, GL_FLOAT, (void*) offsetof(draw_dat3d_t, aTexId) },
	{ 0, 2, GL_FLOAT, (void*) offsetof(draw_dat3d_t, aTexUVPos) },
	{ 0, 2, GL_FLOAT, (void*) offsetof(draw_dat3d_t, aTexUVSize) }
};
