varying vec3 normal;
varying vec3 vertex_to_light_vector;
varying vec2 texcoord;

attribute vec2 position;

mat4 scale(float x, float y, float z) {
    return mat4(
        vec4(x,   0.0, 0.0, 0.0),
	vec4(0.0, y,   0.0, 0.0),
	vec4(0.0, 0.0, z,   0.0),
	vec4(0.0, 0.0, 0.0, 1.0));
}

void main()
{
    gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
    normal = gl_NormalMatrix * gl_Normal;
    vec4 vertex_in_modelview_space = gl_ModelViewMatrix * gl_Vertex;
    vertex_to_light_vector = vec3(gl_LightSource[0].position - vertex_in_modelview_space);
    texcoord = position * vec2(0.5) + vec2(0.5);
}
