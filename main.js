const leniaSource = `
attribute vec2 vertexPos;
void main(void) { gl_Position = vec4(vertexPos, 0., 1.); }

=====

precision highp float;
uniform float iTime;
uniform vec4 iResolution;
uniform vec4 iMouse;
uniform sampler2D iChannel0;



// modified from https://www.shadertoy.com/view/7ls3z7

// TODO: Extract all these to uniforms, specified in JS. (From a JSON description of the level.)
const float R = 5.;       // space resolution = kernel radius
const float T = 5.;       // time resolution = number of divisions per unit time
const float dt = 1./T;     // time step
const vec3 mu = vec3(0.4, .2, .4);     // growth center // TODO: First two worlds.
const vec3 sigma = vec3(.08, .04, .1); // growth width // TODO: First two worlds.
// const vec3 mu = vec3(0.12, 0.6, .2);     // growth center // TODO: Fountains.
// const vec3 sigma = vec3(.08, .2, .05); // growth width // TODO: Fountains.
const vec3 rho = vec3(1.2, .5, .5);     // kernel center
const vec3 omega = vec3(.1, .14, .14);  // kernel width
const mat3 mixing = mat3(
    /* Fountains of power (really needs something that forces the player to move, preferably to another side):
        0.5, -0.577,  -6.0,
        0.5,    2.0,  -1.0,
       -2.0,   -1.0,  0.63/**/
    /* Eye of the storm (puzzle: have to realize the need to stand in spinny wave corners):
        1.,-5., .5,
        .1, 1.,-1.1,
    -5., .5, 1./**/
    /* Self-eating waves (suitable for battles):*/
        .6, -10.,  .75,
       -1., 2.02,  -1.,
       -8.,   .8,   1./**/
);
const vec2 offset       = vec2(0., -9.); // wind
const vec2 kernelOffset = vec2(0., -9.); // wind for the kernel center (originally from a bugged wind implementation; looked too cool to fix)

vec3 bell(vec3 x, vec3 m, vec3 s) {
    return exp(-(x-m)*(x-m)/s/s/2.);  // bell-shaped curve
}

vec3 lenia(in sampler2D prev, in mat3 channels, in vec2 fragCoord) {
    vec3 sum = vec3(0.);
    vec3 total = vec3(0.);
    for (int y=-int(R); y<=int(R); y++)
    for (int x=-int(R); x<=int(R); x++)
    {
        vec2 xy = vec2(x,y);
        vec3 r = vec3(length(xy - kernelOffset) / R);
        vec2 txy = mod((fragCoord + xy - offset) / iResolution.xy, 1.);
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

    // TODO: How to move this actor system to, uh, uniform arrays or textures or something? And, be some linear equation from numbers like "time" and "colorNearby" (health) and "colorGrad" (collision) and "distToMouse" (player control) and "distToTargetN" (2 CPU-specified indices) and speed and health and score and emittances and 1 to movement of position and health-change and score-change and emittances.
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



// TODO: Separate levels. Into JSON files!
//   You are next.
//   (With a Markdown description, and "is this the main-menu/hub, meaning that we don't need time+score and need level-select buttons"; lenia: width&height, simulation params, the color matrix, all actor behavior matrices & initial state and "is this yours" and target-selecting JS, and "how much score to win this level, and what levels would this level unlock, by path".)
//   TODO: Load levels with `fetch` and `JSON.parse`.
//   TODO: Have a directory for levels, with the loaded-by-default being `levels/initial.json`.


// TODO: An actor system.
//   TODO: Expose several numeric variables to actors, and make each actor's behavior (dx, dy, dhealth, dscore, and emittances) just a matrix multiplication.
//   TODO: A vertex shader should emit a square of possible-emittance for each actor. The world is like a special fullscreen actor.
//   TODO: Each frame, download health and score and position from GPU.
// TODO: An actor-health system, communicating GPU->CPU to know which ones to kill (and update GPU data when that happens).
// TODO: An actor-target system, making JS decide the index of the target.

// TODO: Display in-level time and score, and healthbars for each of "your" agents.
// TODO: The main menu, with "start" (first level) and "continue" (using the save file from localStorage, with at least the unlocked levels; select the level, with a hierarchical view).
//   TODO: And "settings", which at least prompts whether to make this level the default.

// TODO: ...With an actor system, and a player-health system (and/or an actor health system, to kill enemies), come up with concrete levels.
// Levels, the meat of the game, allowing dynamic discoveries of whole different worlds of complexity.
//   Eye of the storm, 512×512 (blue must hurt, green can slowly heal cause it's close to blue OR red can heal cause it's far from blue, and in eyes) (actor radius 50 here):
//     `rgb.rgb += vec3(.0, .2, .0)`, moving linearly in a direction for 1 second:
//       Creates actual spiral-eyes, without too much complexity. Perfect.
//     `rgb.rgb += vec3(.0, .2, .0)`, blinking with times 5s on, 1s off:
//       A big wave. Each period, the wave changes color. Eventually, everything descends into chaos.
//     (Nothing else seems to have interesting behavior.)
//     `offset=(0,-2)`: dripping shifting platforms: red space, blue supports, green grass.
//     `offset=(0,3)`: eternal bullet-hell, with diagonal blue bullets in a sea of red, and green explosions.
//     `offset=(0,4)`: green becomes a self-regenerating eternal wave; if the player can burst blue for a bit, it burns away the green.
//     `offset=kernelOffset=(0,-2)`:
//       Blue: a really cool animation. Could be good for the title screen.
//       Green+blue: violently-shifting platforms, too hard to climb, but cool visually.
//     `offset=kernelOffset=(0,-3)`:
//       Blue: blue gliders with red wings, fighting for the right to become a platform.
//       Green: first a green field with blue sparkles, then slowly-shifting screen-wide platforms, too hard to climb.
//     `offset=kernelOffset=(0,-4)`:
//       Blue: glacial blue gliders held up by red flames, which sometimes flare up and destroy other gliders; gliders sometimes split off.
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
//     offset=kernelOffset=(0,-1), with `blue * perc`: a very complex animation, with blue mushroom tops always rising up whenever the red goes away from the actor.
//     offset=kernelOffset=(0,-2), with `blue * perc`: blue releases much more rapidly, and the red in its wake looks like blood dripping down.
//       (Platformer, anyone?)
//     offset=kernelOffset=(0,-5), with `green`: levitating, blood-dripping caves with slight cave-ins.
//       (With actors doing some collision detection to stay out of terrain, and taking damage if impossible, getting to the top can be a challenging task.)
//       `red * perc` functions as a cave-in aura.
//     `offset=(0,-1)`, blue: a colony has come to kill you; weave through the little things.
//     `offset=(0,-2)`, green: self-replicating gliders are death, green grass that can be on top of them is life, in a platformer that depends on either the player sometimes injecting green or some actors doing so. If greenery is unneeded, blue actors can act as wipers.
//     `offset=(0,-4)`, green+blue: title-screen material, with full-screen waves, green held up by blue.
//     `offset=(0,-5)`, green: green-triangle churn, and blue-bullet hell.
//     `offset=(0,-6)`:
//       Blue, constant: a being. For a cutscene, maybe?
//       Blue+green: a trapped green being.
//       Green: the great green being.
//   Fountains of power:
//     `dx=0, dy=-9`, with `blue * perc` and `mu=.2, sigma=.05`: builds indistinct Sierpinski triangles, until they reach the actor and destroy their own source, and start again.
//       (Could be the great barrier to cross.)
//       (A small actor radius makes the triangle much more distinct.)
//       `mu=.6, sigma=.2`: makes puffs that slowly dissipate, but if the actor stays long enough to make top and bottom connect, the whole world is taken over.
//     `offset=kernelOffset=(0,-11)`: the barrier's building has visible sparks, and multiple layers, and looks cool.


const displaySource = `
attribute vec2 vertexPos;
void main(void) { gl_Position = vec4(vertexPos, 0., 1.); }

=====

precision highp float;
uniform vec4 iDisplay;
uniform vec4 iResolution;
uniform sampler2D iChannel0;

// TODO: Make this a uniform, specified in the level.
const mat4 colorTransform = mat4(
    1., 0., 0., 0.,
    0., 1., 0., 0.,
    0., 0., 1., 0.,
    0., 0., 0., 1.
);

void main() {
    // STRETCH
    gl_FragColor = texture2D(iChannel0, (gl_FragCoord.xy) / iDisplay.xy) * colorTransform;
}`



const mouse = { x:0, y:0, main:false, aux:false, update(evt) {
    mouse.x = (evt.clientX + (evt.movementX || 0)) / innerWidth
    mouse.y = (evt.clientY + (evt.movementY || 0)) / innerHeight
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
    const physicsW = 512, physicsH = 512 // TODO: Put these into the level state.
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
            'iDisplay',
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
            gl.uniform4f(u.iResolution, physicsW, physicsH, 0, 0)
            gl.uniform4f(u.iDisplay, gl.drawingBufferWidth, gl.drawingBufferHeight, 0, 0)
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