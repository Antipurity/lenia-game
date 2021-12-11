Seems to be viable, though with gimmicky gameplay: dodge the green colors, looks like.

(I guess without a neural interface, to shoot properly, would need images of what you're imposing on the world. Or at least particles.)

```glsl
// Noise simplex 2D by iq - https://www.shadertoy.com/view/Msf3WH

vec2 hash( vec2 p )
{
	p = vec2( dot(p,vec2(127.1,311.7)), dot(p,vec2(269.5,183.3)) );
	return -1.0 + 2.0*fract(sin(p)*43758.5453123);
}

float noise( in vec2 p )
{
    const float K1 = 0.366025404; // (sqrt(3)-1)/2;
    const float K2 = 0.211324865; // (3-sqrt(3))/6;

	vec2  i = floor( p + (p.x+p.y)*K1 );
    vec2  a = p - i + (i.x+i.y)*K2;
    float m = step(a.y,a.x); 
    vec2  o = vec2(m,1.0-m);
    vec2  b = a - o + K2;
	vec2  c = a - 1.0 + 2.0*K2;
    vec3  h = max( 0.5-vec3(dot(a,a), dot(b,b), dot(c,c) ), 0.0 );
	vec3  n = h*h*h*h*vec3( dot(a,hash(i+0.0)), dot(b,hash(i+o)), dot(c,hash(i+1.0)));
    return dot( n, vec3(70.0) );
}
```
```glsl
// modified from https://www.shadertoy.com/view/7ls3z7

uniform sampler2D iChannel0;

const float R = 6.;       // space resolution = kernel radius
const float T = 5.;       // time resolution = number of divisions per unit time
const float dt = 1./T;     // time step
const vec3 mu = vec3(0.4, .2, .4);     // growth center
const vec3 sigma = vec3(.08, .04, .1); // growth width
const vec3 rho = vec3(1.2, .5, .5);     // kernel center
const vec3 omega = vec3(.1, .14, .14);  // kernel width

vec3 bell(vec3 x, vec3 m, vec3 s)
{
    return exp(-(x-m)*(x-m)/s/s/2.);  // bell-shaped curve
}

vec3 lenia(in sampler2D prev, in mat3 channels, in vec2 fragCoord) {
    vec2 uv = fragCoord / iResolution.xy;

    vec3 sum = vec3(0.);
    vec3 total = vec3(0.);
    for (int x=-int(R); x<=int(R); x++)
    for (int y=-int(R); y<=int(R); y++)
    {
        vec3 r = vec3(sqrt(float(x*x + y*y)) / R);
        vec2 txy = mod((fragCoord + vec2(x,y)) / iResolution.xy, 1.);
        vec3 val = texture(prev, txy).rgb * channels;
        vec3 weight = bell(r, rho, omega);
        sum += val * weight;
        total += weight;
    }
    vec3 avg = sum / total;

    vec3 val = texture(prev, uv).rgb;
    vec3 growth = bell(avg, mu, sigma) * 2. - 1.;
    return clamp(val + dt * growth, 0., 1.);
}

void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    mat3 mixing = mat3(
        /*Eye of the storm (puzzle: have to realize the need to stand in spinny wave corners):*/
         1.,-5., .5,
         .1, 1.,-1.1,
        -5., .5, 1./**/
        /*Self-eating waves (suitable for battles):
         .6,-10., .75,
        -1., 2.,-1.,
        -8., .8, 1./**/
    );
    vec3 rgb = lenia(iChannel0, mixing, fragCoord);

    if (iMouse.z > 0.) {
        float d = length((fragCoord.xy - iMouse.xy) / iResolution.xx);
        float maxD = 50., perc = 1. - d / maxD*iResolution.x;
        float p = .9;
        if (d <= maxD/iResolution.x) {
            float x = sin(iTime*2.) + sin(iTime*3.) + sin(iTime*13.);
            float y = sin(iTime*5.) + sin(iTime*7.) + sin(iTime*11.);
        	//rgb.r = rgb.r*p + abs(noise(fragCoord/100. + 0.*.99*vec2(x,y)) * perc);
        	//rgb.g = rgb.g*p + abs(noise(fragCoord/10. + 0.*2.01*vec2(x,y)) * perc);
        	//rgb.b = rgb.b*p + abs(noise(fragCoord/1. + 0.*3.05*vec2(x,y)) * perc);
            rgb.rgb = rgb.rgb*p + vec3(perc*sin(iTime*3.4)*.1, perc*sin(iTime*6.1)*.3, perc*sin(iTime*1.)*.1);
        }
    }

    fragColor = vec4(rgb,1.);
}
```