varying vec3 normal;
varying vec3 vertex_to_light_vector;

void main()
{
    const vec4 AmbientColor = vec4(0.0, 0.0, 0.1, 1.0);
    const vec4 DiffuseColor = vec4(0.0, 0.8, 0.0, 1.0);

    float DiffuseTerm = clamp(
       dot(normalize(normal),
           normalize(vertex_to_light_vector)), 0.0, 1.0);
    
    vec4 color;

    if(DiffuseTerm > 0.90)
        color = vec4(1.0, 0.5, 0.5, 1.0);
    else if (DiffuseTerm > 0.5)
    	color = vec4(0.6, 0.3, 0.3, 1.0);
    else if (DiffuseTerm > 0.35)
        color = vec4(0.4, 0.2, 0.2, 1.0);
    else
        color = vec4(0.2, 0.1, 0.1, 1.0);

    gl_FragColor = color;
}
