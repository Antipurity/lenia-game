const leniaSource = `
attribute vec2 vertexPos;
void main(void) { gl_Position = vec4(vertexPos, 0., 1.); }

=====

precision highp float;
uniform float iTime;
uniform vec4 iResolution;
uniform vec4 iMouse;
uniform sampler2D iChannel0;



// Noise simplex 2D by iq - https://www.shadertoy.com/view/Msf3WH

vec2 hash( vec2 p ) {
	p = vec2( dot(p,vec2(127.1,311.7)), dot(p,vec2(269.5,183.3)) );
	return -1.0 + 2.0*fract(sin(p)*43758.5453123);
}

float noise( in vec2 p ) {
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



// modified from https://www.shadertoy.com/view/7ls3z7

// TODO: Extract all these to uniforms, specified in JS. (From a JSON description of the level.)
const float R = 5.;       // space resolution = kernel radius
const float T = 5.;       // time resolution = number of divisions per unit time
const float dt = 1./T;     // time step
const vec3 mu = vec3(0.4, 0.6, .2);     // growth center // TODO: First two worlds: vec3(0.4, .2, .4)
const vec3 sigma = vec3(.08, .2, .05); // growth width // TODO: First two worlds: vec3(.08, .04, .1)
const vec3 rho = vec3(1.2, .5, .5);     // kernel center
const vec3 omega = vec3(.1, .14, .14);  // kernel width
const mat3 mixing = mat3(
    /* Fountains of power (really needs something that forces the player to move, preferably to another side):*/
        1.0,     .0,   0.3,
         .0,    2.0,  -1.0,
        0.0,   -1.0,  0.63/**/
    /* Eye of the storm (puzzle: have to realize the need to stand in spinny wave corners):
        1.,-5., .5,
        .1, 1.,-1.1,
    -5., .5, 1./**/
    /* Self-eating waves (suitable for battles):
        .6,-10., .75,
    -1., 2.,-1.,
    -8., .8, 1./**/
);

vec3 bell(vec3 x, vec3 m, vec3 s) {
    return exp(-(x-m)*(x-m)/s/s/2.);  // bell-shaped curve
}

vec3 lenia(in sampler2D prev, in mat3 channels, in vec2 fragCoord) {
    vec3 sum = vec3(0.);
    vec3 total = vec3(0.);
    for (int y0=-int(R); y0<=int(R); y0++)
    for (int x0=-int(R); x0<=int(R); x0++)
    {
        int y = y0 - 9;
        int x = x0 + 0; // TODO: Extract x/y shifts into uniforms. Different for each color. (Could even do stuff like "point an actor's emissions towards the player".) (...Probably no need for per-pixel offsets, right? Yeah, I don't think so.)
        vec3 r = vec3(sqrt(float(x*x + y*y)) / R);
        vec2 txy = mod((fragCoord + vec2(x,y)) / iResolution.xy, 1.);
        vec3 val = texture2D(prev, txy).rgb * channels;
        vec3 weight = bell(r, rho, omega);
        sum += val * weight;
        total += weight;
    }
    vec3 avg = sum / total;

    vec3 val = texture2D(prev, fragCoord / iResolution.xy).rgb;
    vec3 growth = bell(avg, mu, sigma) * 2. - 1.;
    return clamp(val + dt * growth, 0., 1.);
}

void main(void) {
    vec2 coord = gl_FragCoord.xy; // 0…1
    vec3 rgb = lenia(iChannel0, mixing, coord);

    // TODO: How to move this actor system to, uh, uniform arrays or textures or something? And, be some linear equation from numbers like "time" and "colorNearby" (health) and "colorGrad" (collision) and "distToMouse" (player control) and "distToTarget" (CPU-specified index) to movement...
    // TODO: And, how to have more diverse actors? Like, do shapes matter? A bit, in some special circumstances... But I think just a linear equation will suffice, right?
    if (iMouse.z > 0.) {
        float d = length((coord.xy - iMouse.xy) / iResolution.xy);
        float maxD = 50., perc = 1. - d / maxD*iResolution.x;
        if (d <= maxD/iResolution.x) {
            rgb.rgb += vec3(.0, .2, .0) * perc;
        }
    }
    if (iMouse.w > 0.) {
        float d = length((coord - iMouse.xy) / iResolution.xy);
        float maxD = 50., perc = 1. - d / maxD*iResolution.x;
        if (d <= maxD/iResolution.x) {
            rgb.rgb += vec3(.0, .0, .2) * perc;
        }
    }

    gl_FragColor = vec4(rgb, 1.);
}`
// TODO: Research how we can apply velocity to the whole level, in a way that actually looks natural, not like shit.
//   (Might be useful for open-world adventures, with actors preserved but colors discarded, because it's quite boring otherwise. ...Also really good for animations.)



// TODO: Separate levels. Into JSON files!
//   (With simulation params, all actor descriptions (including "what hurts it and by how much") & possibly code for each actor group.)
// TODO: Document interesting actor behaviors in levels. (They would be the meat of the game, allowing dynamic discoveries of whole different worlds.)
//   Eye of the storm, 512×512 (blue must hurt, green can slowly heal cause it's close to blue OR red can heal cause it's far from blue, and in eyes) (actor radius 50 here):
//     `rgb.rgb += vec3(.0, .2, .0)`, moving linearly in a direction for 1 second:
//       Creates actual spiral-eyes, without too much complexity. Perfect.
//     `rgb.rgb += vec3(.0, .2, .0)`, blinking with times 5s on, 1s off:
//       A big wave. Each period, the wave changes color. Eventually, everything descends into chaos.
//     (Nothing else seems to have interesting behavior.)
//     With `dx=+2, dy=0`:
//       `rgb.rgb += vec3(.0, .0, .2)`: a really cool animation. Not a level though.
//       (Unplayable, because the storm has no safe spaces.)
//     With `dx=+1, dy=0`:
//       `rgb.rgb += vec3(.0, .0, .2)`: an even more cool animation. Could be good for the title screen.
//   Self-eating waves, 1024×1024:
//     `rgb.rgb += vec3(.0, .2, .0)` green circles, of radii such as 10/50/100/500:
//       Init:
//         When unmoving on black, creates a big green bubble.
//         When moving, creates green->blue->red transitions.
//         When unmoving but blinking, creates waves on each creation.
//       When the field is filled with shimmering blue circles, which impede green ones:
//         Borders emanate blue.
//         Moving slowly creates no disturbances, only a cool trail.
//         Moving quickly creates green waves, which dissipate.
//         Radius 100: highlights its movement history in blue with bubble-like circles, looking cool.
//     `rgb.rgb += vec3(.0, .0, .2)` blue circles, radius 50 tested:
//       Init:
//         Sometimes initiates colonies of dividing blue cells.
//       Moving:
//         Erases blue colonies.
//         Looks entwined with red. Leaves some red behind; much more when ` * perc`.
//     `rgb.rgb += vec3(.2, .0, .0)` red circles, radius 50 tested:
//       In black or in green:
//         Does nothing. A faintly-red blob.
//       In blue:
//         When staying, the blue slowly ignites into intense red, and the ignited material's movement leaves a trail, but can't keep up if the actor moves too fast.
//           (When ` * perc`, there's no ignition.)
//         A battery for greenery.
//     `dx=0, dy=+1`, with `blue * perc`: a very complex animation, with blue mushroom tops always rising up whenever the red goes away from the actor.
//     `dx=0, dy=+2`, with `blue * perc`: blue releases much more rapidly, and the red in its wake looks like blood dripping down.
//       (Platformer, anyone?)
//     `dx=0, dy=+5`, with `green`: levitating, blood-dripping caves with slight cave-ins.
//       (With actors doing some collision detection to stay out of terrain, and taking damage if impossible, getting to the top can be a challenging task.)
//       `red * perc` functions as a cave-in aura.
//   Fountains of power:
//     `dx=0, dy=-9`, with `blue * perc` and `mu=.2, sigma=.05`: builds indistinct Sierpinski triangles, until they reach the actor and destroy their own source, and start again.
//       (Could be the great barrier to cross.)
//       (A small actor radius makes the triangle much more distinct.)
//       `dy=-8`: makes the arms big enough to cross the screen multiple times before the middle is reached and everything restarts.
//       `mu=.6, sigma=.2`: makes puffs that slowly dissipate, but the actor stays long enough to make top and bottom connect, the whole world is taken over.


// TODO: An actor system.
//   TODO: Expose several numeric variables to actors, and make each actor's behavior just a matrix multiplication.
// TODO: An actor-health system, communicating GPU->CPU to know which ones to kill (and update GPU data when that happens).
// TODO: An actor-target system, making JS decide the index of the target.
// TODO: ...With an actor system, and a player-health system (and/or an actor health system, to kill enemies), come up with concrete levels.


const displaySource = `
attribute vec2 vertexPos;
void main(void) { gl_Position = vec4(vertexPos, 0., 1.); }

=====

precision highp float;
uniform vec4 iResolution;
uniform sampler2D iChannel0;
void main() {
    // STRETCH
    gl_FragColor = texture2D(iChannel0, gl_FragCoord.xy / iResolution.xy);
}`



const mouse = { x:0, y:0, main:false, aux:false, update(evt) {
    const dpr = (self.devicePixelRatio || 1) | 0
    mouse.x = (evt.clientX + (evt.movementX || 0)) / innerWidth * dpr
    mouse.y = (evt.clientY + (evt.movementY || 0)) / innerHeight * dpr
    mouse.main = evt.buttons & 1
    mouse.aux = evt.buttons & 2
} }
addEventListener('pointerdown', mouse.update, {passive:true})
addEventListener('pointermove', mouse.update, {passive:true})
addEventListener('pointerup', mouse.update, {passive:true})
addEventListener('contextmenu', evt => evt.preventDefault())





loop(document.getElementById('main'))
function loop(canvas) {
    // The main drawing loop.
    if (!canvas.gl)
        canvas.gl = canvas.getContext('webgl', {alpha:false, desynchronized:true})
    const gl = canvas.gl
    canvas.width = canvas.height = 0
    const physicsW = 512, physicsH = 512 // TODO:
    const glState = {
        leniaPhysics: null,
        display: null,
        posBuffer: null,
        prevLeniaFrame: null,
        nextLeniaFrame: null,
    }

    canvas.addEventListener('webglcontextlost', evt => evt.preventDefault()) // Allow restoring.
    canvas.addEventListener('webglcontextrestored', setup)

    setup()
    draw()
    function setup() {
        const s = glState
        s.leniaPhysics = initShaders(gl, leniaSource.split('====='), [
            'iTime',
            'iResolution',
            'iMouse',
            'iChannel0',
        ], [
            'vertexPos',
        ])
        s.display = initShaders(gl, displaySource.split('====='), [
            'iResolution',
            'iChannel0',
        ], [
            'vertexPos',
        ])
        s.posBuffer = initBuffer(gl, [-1,1, 1,1, -1,-1, 1,-1], 2)
        s.prevLeniaFrame = initTexture(gl, physicsW, physicsH)
        s.nextLeniaFrame = initTexture(gl, physicsW, physicsH)
        gl.clearColor(0,0,0,1)
    }
    function draw() {
        requestAnimationFrame(draw)
        maybeResize(canvas, canvas)
        gl.clear(gl.COLOR_BUFFER_BIT)
        const s = glState, p1 = s.leniaPhysics, p2 = s.display, rect = s.posBuffer
        if (p1 !== null) {
            const u = p1.uniform, a = p1.attrib
            gl.useProgram(p1.program)
            // Fill in the uniforms.
            gl.uniform1f(u.iTime, performance.now())
            gl.uniform4f(u.iResolution, physicsW, physicsH, 0, 0)
            gl.uniform4f(u.iMouse, mouse.x * physicsW, (1 - mouse.y) * physicsH, mouse.main, mouse.aux)

            // Compute the next frame.
            s.prevLeniaFrame.useRead(gl, 0, u.iChannel0)
            s.nextLeniaFrame.useWrite(gl)

            // Draw the fullscreen rectangle.
            rect.draw(gl, a.vertexPos)

            s.nextLeniaFrame.resetWrite(gl)
        }
        if (p2 !== null) { // Draw the physics.
            const u = p2.uniform, a = p2.attrib
            gl.useProgram(p2.program)
            gl.uniform4f(u.iResolution, gl.drawingBufferWidth, gl.drawingBufferHeight, 0, 0)
            s.nextLeniaFrame.useRead(gl, 0, u.iChannel0)
            rect.draw(gl, a.vertexPos)
        }

        // Commit.
        ;[s.prevLeniaFrame, s.nextLeniaFrame] = [s.nextLeniaFrame, s.prevLeniaFrame]
        gl.flush()
    }
}



function maybeResize(canvas, sizeToElem) {
    const dpr = (self.devicePixelRatio || 1) | 0
    const w = sizeToElem.clientWidth * dpr
    const h = sizeToElem.clientHeight * dpr
    if (canvas.width !== w || canvas.height !== h) {
        canvas.width = w, canvas.height = h
        const gl = canvas.gl
        gl && gl.viewport(0,0, gl.drawingBufferWidth, gl.drawingBufferHeight)
        return true
    }
    return false
}

function initShaders(gl, [vsSource, fsSource], uniformNames=null, attribNames=null) {
    // Compiles vertex+fragment shaders in a WebGL context.
    const vs = initShader(gl, gl.VERTEX_SHADER, vsSource)
    const fs = initShader(gl, gl.FRAGMENT_SHADER, fsSource)
    if (vs === null || fs === null) return null
    const program = gl.createProgram()
    gl.attachShader(program, vs)
    gl.attachShader(program, fs)
    gl.linkProgram(program)
    gl.validateProgram(program)
    if (!gl.getProgramParameter(program, gl.LINK_STATUS)) {
        error(gl.getProgramInfoLog(program))
        gl.deleteProgram(program)
        return null
    }
    const r = { program, uniform:Object.create(null), attrib:Object.create(null) }
    if (uniformNames)
        for (let u of uniformNames)
            r.uniform[u] = gl.getUniformLocation(program, u) // -1 if not found.
    if (attribNames)
        for (let u of attribNames)
            r.attrib[u] = gl.getAttribLocation(program, u) // -1 if not found.
    return r

    function initShader(gl, type, source) {
        const sh = gl.createShader(type)
        gl.shaderSource(sh, source)
        gl.compileShader(sh)
        if (!gl.getShaderParameter(sh, gl.COMPILE_STATUS)) {
            error(gl.getShaderInfoLog(sh))
            gl.deleteShader(sh)
            return null
        }
        return sh
    }
}

function initBuffer(gl, f32, numbersPerValue = 1, usageHint = gl.STATIC_DRAW) {
    // An array of f32 values, `r`.
    //   Read with `r.use(gl, gl.getAttribLocation(program, 'attrib'))` as a vertex attribute.
    //   Draw this list of vertices with `r.draw(gl, gl.getAttribLocation(program, 'attrib'))`.
    if (!(f32 instanceof Float32Array)) f32 = Float32Array.from(f32)
    const buf = gl.createBuffer()
    gl.bindBuffer(gl.ARRAY_BUFFER, buf)
    gl.bufferData(gl.ARRAY_BUFFER, f32, usageHint)
    return {
        buf,
        length: f32.length,
        numbersPerValue,
        use(gl, attribLocation) {
            gl.bindBuffer(gl.ARRAY_BUFFER, this.buf)
            gl.vertexAttribPointer(attribLocation, this.numbersPerValue, gl.FLOAT, false, 0, 0)
            gl.enableVertexAttribArray(attribLocation)
        },
        draw(gl, attribLocation) {
            this.use(gl, attribLocation)
            gl.drawArrays(gl.TRIANGLE_STRIP, 0, this.length / this.numbersPerValue | 0)
        },
    }
}

function initTexture(gl, width, height) {
    // A 2D array of RGBA values `r`.
    //   Read with `r.useRead(gl, 0, gl.getUniformLocation(program, 'textureName'))` in JS,
    //     `uniform sampler2D textureName;  void main(void) { texture2D(textureName, vec2(0., 0.)) }` in GLSL.
    //   Write with `r.useWrite(gl)`, then finally `r.resetWrite(gl)`.
    //     (Can only write to 1 texture at a time.)
    //     Reset with `gl.bindFramebuffer(gl.FRAMEBUFFER, null), gl.viewport(0,0, gl.canvas.width, gl.canvas.height)`.
    const tex = gl.createTexture()
    gl.bindTexture(gl.TEXTURE_2D, tex)
    gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, width, height, 0, gl.RGBA, gl.UNSIGNED_BYTE, null)
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR)

    const fb = gl.createFramebuffer()
    gl.bindFramebuffer(gl.FRAMEBUFFER, fb)
    gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_2D, tex, 0)
    return { // Who needs JS classes when you have *objects*? What are we aiming for anyway, *efficiency* or something?
        read: tex,
        write: fb,
        _width: width,
        _height: height,
        useRead(gl, i, uniformLocation) {
            gl.activeTexture(i === this._i ? this._o : (this._i = i, this._o = gl['TEXTURE'+i]))
            gl.bindTexture(gl.TEXTURE_2D, this.read)
            gl.uniform1i(uniformLocation, i)
        },
        useWrite(gl) {
            gl.bindFramebuffer(gl.FRAMEBUFFER, this.write)
            gl.viewport(0,0, this._width, this._height)
        },
        resetWrite(gl) {
            gl.bindFramebuffer(gl.FRAMEBUFFER, null)
            gl.viewport(0,0, gl.canvas.width, gl.canvas.height)
        },
    }
}

function error(...msg) { console.error(...msg) }