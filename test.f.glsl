varying vec3 normal;
varying vec3 vertex_to_light_vector;
varying vec2 texcoord;

uniform sampler2D tex;

float cel(float d) {
    return smoothstep(0.35, 0.37, d) * 0.4 + smoothstep(0.70, 0.72, d) * 0.6;
}

float warp_diffuse(float d) {
    return cel(d * 0.5 + 0.5);
}

void main()
{
    const vec4 AmbientColor = vec4(0.0, 0.0, 0.1, 1.0);
    const vec4 DiffuseColor = vec4(0.3, 0.7, 0.3, 1.0);
    float dp = max(dot(normalize(normal), normalize(vertex_to_light_vector)), 0.0);
    float DiffuseTerm = warp_diffuse(dp);
    gl_FragColor = DiffuseColor * DiffuseTerm;    
}
