const R = 5
const leniaSource = `
attribute vec2 vertexPos;
void main() { gl_Position = vec4(vertexPos, 0., 1.); }

=====

precision highp float;
uniform float iTime;
uniform vec4 iResolution;
uniform vec4 iMouse;

uniform sampler2D leniaGrid;
uniform sampler2D leniaKernel;



// modified from https://www.shadertoy.com/view/7ls3z7

// TODO: Extract all these to levels. (Kinda need that actor system, so that we could actually see what those levels look like.)
// const vec3 mu = vec3(0.4, .2, .4);     // growth center // TODO: First two worlds.
// const vec3 sigma = vec3(.08, .04, .1); // growth width // TODO: First two worlds.
// // const vec3 mu = vec3(0.12, 0.6, .2);     // growth center // TODO: Fountains.
// // const vec3 sigma = vec3(.08, .2, .05); // growth width // TODO: Fountains.
// const mat3 mixing = mat3(
//     /* Fountains of power (really needs something that forces the player to move, preferably to another side):
//         0.5, -0.577,  -6.0,
//         0.5,    2.0,  -1.0,
//        -2.0,   -1.0,  0.63/**/
//     /* Eye of the storm (puzzle: have to realize the need to stand in spinny wave corners):*/
//         1.,-5., .5,
//         .1, 1.,-1.1,
//        -5., .5, 1./**/
//     /* Self-eating waves (suitable for battles):
//         .6, -10.,  .75,
//        -1., 2.02,  -1.,
//        -8.,   .8,   1./**/
// );

uniform float iSlowdown;
uniform mat3 iMixing;
uniform vec3 iGrowthCenter;
uniform vec3 iGrowthWidth;
uniform vec2 iOffset;
uniform vec2 iKernelOffset;

const int R = ${R}; // GLSL can only loop with constant bounds.

vec3 bell(vec3 x, vec3 m, vec3 s) {
    return exp(-(x-m)*(x-m)/(s*s*2.));  // bell-shaped curve
}

vec3 lenia(in sampler2D prev, in mat3 channels, in vec2 fragCoord) {
    vec3 sum = vec3(0.);
    vec3 total = vec3(0.);
    for (int y=-R; y<=R; y++)
    for (int x=-R; x<=R; x++)
    { // Convolution with a pre-computed kernel.
        vec2 xy = vec2(x,y);
        vec2 txy = mod((fragCoord + xy - iOffset) / iResolution.xy, 1.);
        vec3 val = texture2D(prev, txy).rgb * channels;
        vec3 weight = texture2D(leniaKernel, (xy + float(R)) * ${1 / (2*R)}).rgb;
        sum += val * weight;
        total += weight;
    }
    vec3 avg = sum / total;

    vec3 val = texture2D(prev, fragCoord / iResolution.xy).rgb;
    vec3 growth = bell(avg, iGrowthCenter, iGrowthWidth) * 2. - 1.;
    return clamp(val + growth / iSlowdown, 0., 1.);
}

void main() {
    vec2 coord = gl_FragCoord.xy; // 0…1
    vec3 rgb = lenia(leniaGrid, iMixing, coord);

    // TODO: How to move this actor system to, uh, uniform arrays or textures or something? And, be some linear equation from numbers like "time" and "colorNearby" (health) and "colorGrad" (collision) and "distToMouse" (player control) and "distToTargetN" (2 CPU-specified indices) and speed and health and score and emittances and 1 to movement of position and health-change and score-change and emittances.
    // TODO: ...Is it possible to make actors be able to shoot other actors... Teleport a hidden actor to a position, maybe?
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



const displaySource = `
attribute vec2 vertexPos;
void main() { gl_Position = vec4(vertexPos, 0., 1.); }

=====

precision highp float;
uniform vec4 iDisplay;
uniform vec4 iResolution;
uniform sampler2D leniaGrid;
uniform sampler2D leniaKernel;

uniform mat4 iColorMatrix;

void main() {
    // STRETCH
    gl_FragColor = texture2D(leniaGrid, gl_FragCoord.xy / iDisplay.xy) * iColorMatrix;
    // gl_FragColor = texture2D(leniaKernel, gl_FragCoord.xy / iDisplay.xy) * iColorMatrix; // Visualize the kernel.
}`



const actorSource = `
precision highp float;

// TODO: Also iTime.
uniform vec4 iResolution;
uniform sampler2D leniaGrid;

attribute vec4 posSpeed; // x/y/dx/dy
attribute vec4 extraState; // health/score/dummy1/dummy2
attribute float emitRadius;

varying vec4 outPosSpeed; // x/y/dx/dy
varying vec4 outExtraState; // health/score/dummy1/dummy2
varying vec4 emittance;

void main() {
    // TODO: Compute behavior, as matrix multiplications.
    //   TODO: Put behavior-matrix rows into a (square) texture, to be read here.
    //   (Outputs: dx, dy, dhealth, dscore; emitR, emitG, emitB. If not specified, the row is 0s, meaning that the output is always 0.)
    //     (Inputs: 1, x, y, dx, dy, health, score, dummy1, dummy2, r,rx,ry, g,gx,gy, b,bx,by, sin(time*pi*1), sin(time*pi*2), sin(time*pi*4), TODO:. Eventually, distToTargetX/distToTargetY, distToMouseX/distToMouseY.)
    // TODO: Read close-to-actor colors from the Lenia-state texture, to later give them as inputs to behavior.
    vec2 nextPos = mod(posSpeed.xy + posSpeed.zw, 1.);
    outPosSpeed = vec4(nextPos, posSpeed.zw);
    outExtraState = extraState * vec4(0., 0., 0., 0.);
    emittance = vec4(.0, .0, .3, emitRadius);

    gl_Position = vec4(outPosSpeed.xy * 2. - 1., 0., 1.);
    gl_PointSize = emitRadius;
}

=====

precision highp float;

uniform vec4 iResolution;
uniform sampler2D leniaGrid;

varying vec4 outPosSpeed; // x/y/dx/dy
varying vec4 emittance;

void main() {
    vec2 center = outPosSpeed.xy;
    float distance = length(gl_FragCoord.xy - center * iResolution.xy) / emittance.w * 2.;
    if (distance < 1.) // Relies on blending.
        gl_FragColor = vec4(emittance.rgb, 1.) * (1. - distance);
    else
        discard;
}
`



// TODO: An actor system.
//   TODO: Each frame, download x/y/dx/dy and health and dscore from GPU. Add dscore to score, and *maybe* execute "onDied" JS if health<=0 now.
//   TODO: In the level, the object `actors`, where each actor is named:
//     TODO: pos: [x, y, dx, dy], all 0..1.
//       TODO: If >4 numbers, instantiate many agents, each sharing behavior in the behavior store.
//     TODO: emitRadius: 50.
//     TODO: health. When ≤0, it's not updated anymore.
//     TODO: displayHealth, true/false.
//     TODO: Per-output-variable behavior matrix. Outputs = matmul(outputs, behavior).
//       (Outputs: dx, dy, dhealth, dscore; emitR, emitG, emitB. If not specified, the row is 0s, meaning that the output is always 0.)
//       (Inputs: 1, x, y, dx, dy, health, score, dummy1, dummy2, r,rx,ry, g,gx,gy, b,bx,by, sin(time*pi*1), sin(time*pi*2), sin(time*pi*4), TODO:. Eventually, distToTargetX/distToTargetY, distToMouseX/distToMouseY.)
//     TODO: ...How do we do target-selecting JS (given all of an actor's state), exactly?... Each frame, call up to 1000 targeters, and update in-buffer if updated?
//     TODO: Document actors.
// TODO: An actor-health system, communicating GPU->CPU to know which ones to kill (and update GPU data when that happens --- ...or just ignore it GPU-side), and display it in DOM.
// TODO: An actor-target system, making JS decide the index of the target.
// TODO: ...Do we want DOM-side labels on agents?... (Usable for text boxes, even: STORY. And for the main menu's label.)

// TODO: When score exceeds `level.winScore`, switch to winning state (...what, is the level's JS responsible for it, or what?), then after a time, unlock all levels in `level.winUnlocks` (notifying how many are new) and go to main menu.
// TODO: Display in-level time and score (out of `level.winScore`), and healthbars for each of "your" agents.
// TODO: The main menu, with "start" (first level) and "continue" (using the save file from localStorage, with at least the unlocked levels; select the level, with a hierarchical view).
//   TODO: And "settings", which at least prompts whether to make this level the main-menu default. (Or maybe on visit.)


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
        canvas.gl = canvas.getContext('webgl2', {alpha:false, desynchronized:true})
    const gl = canvas.gl
    if (!gl) throw new Error("Failed to get a WebGL2 context.")
    canvas.width = canvas.height = 0

    let level = null
    loadLevel('levels/initial.json').then(r => level = r).catch(error)

    const glState = {
        // Shader programs.
        leniaPhysics: null,
        actors: null,
        display: null,
        // Buffers.
        posBuffer: null, // For vertices of the full-screen quad.
        posSpeed: null, // Actor x/y/dx/dy.
        extraState: null, // Actor health/dscore/dummy1/dummy2.
        emitRadius: null, // Actor emit-radius.
        // Textures.
        leniaFrames: null, // {prev, next}
        leniaKernel: null,
    }

    canvas.addEventListener('webglcontextlost', evt => evt.preventDefault()) // Allow restoring.
    canvas.addEventListener('webglcontextrestored', setup)

    setup()
    draw()
    function setup() {
        const s = glState
        s.leniaPhysics = initShaders(gl, leniaSource.split('====='), { uniforms:[
            // Simulation parameters.
            'iSlowdown',
            'iMixing',
            'iGrowthCenter', 'iGrowthWidth',
            'iOffset', 'iKernelOffset',
            // Simulation state.
            'iTime',
            'iResolution',
            // Lenia state.
            'leniaGrid',
            'leniaKernel',
            // Debugging.
            'iMouse',
        ], attribs:[
            'vertexPos',
        ] })
        s.actors = initShaders(gl, actorSource.split('====='), { uniforms:[
            'iTime',
            'iResolution',
            'leniaGrid',
        ], attribs:[
            'posSpeed',
            'extraState',
            'emitRadius',
        ], transformFeedback:[
            'outPosSpeed',
            'outExtraState',
        ] })
        s.display = initShaders(gl, displaySource.split('====='), { uniforms:[
            'iColorMatrix',
            'iDisplay',
            'iResolution',
            'leniaGrid',
            'leniaKernel',
        ], attribs:[
            'vertexPos',
        ]})
        s.posBuffer = initBuffer(gl, [-1,1, 1,1, -1,-1, 1,-1], 2)
        s.leniaFrames = null
        gl.clearColor(0,0,0,1)
    }
    function initTextures(s, L) {
        // This is separated from `setup` and called in `draw` after init, because it depends on the level `L`.
        s.leniaFrames = {
            prev:initTexture(gl, L.width, L.height),
            next:initTexture(gl, L.width, L.height), // The Lenia loop modifies Lenia state.
            extra:initTexture(gl, L.width, L.height), // Actors modify Lenia state too.
        }
        if (L.kernel.center)
            s.leniaKernel = leniaKernel(gl, R, L.kernel.center, L.kernel.width, L.iKernelOffset)
        else {
            // Collage .r/.g/.b into one array.
            const sz = 2*R+1, pixels = new Float32Array(4 * sz * sz)
            const rgb = [L.kernel.r || [], L.kernel.g || [], L.kernel.b || []]
            const totals = [0,0,0,1]
            for (let y = -R; y <= R; ++y)
                for (let x = -R; x <= R; ++x) {
                    const index = (y+R) * sz + (x+R)
                    for (let c=0; c < 3; ++c)
                        pixels[4*index + c] = rgb[c][index] || 0,
                        totals[c] = Math.max(totals[c], rgb[c][index] || 0)
                }
            s.leniaKernel = leniaKernel(gl, R, null, null, null, pixels, totals)
        }

        // Load actors.
        const actors = L.actors
        function RANDOM_ARRAY(len, offset=.5, radius=.5) { // TODO: Load this data from the level, don't just decide it randomly.
            const r = new Float32Array(len)
            for (let i=0; i < len; ++i) r[i] = (Math.random()*2-1) * radius + offset
            return r
        }
        s.posSpeed = twice(() => initBuffer(gl, RANDOM_ARRAY(actors.length * 4, 0, .001), 4))
        s.extraState = twice(() => initBuffer(gl, RANDOM_ARRAY(actors.length * 4), 4))
        s.emitRadius = initBuffer(gl, RANDOM_ARRAY(actors.length, 30, 30), 1)
        function twice(f) { return { prev:f(), next:f() } }
    }
    function draw() {
        requestAnimationFrame(draw)
        maybeResize(canvas, canvas)
        gl.clear(gl.COLOR_BUFFER_BIT)
        if (!level) return
        const s = glState, p1 = s.leniaPhysics, p2 = s.actors, p3 = s.display, rect = s.posBuffer, L = level
        if (!s.leniaFrames)
            initTextures(s, L)
        if (p1 !== null) { // Lenia.
            const u = p1.uniform, a = p1.attrib
            gl.useProgram(p1.program)
            // Fill in the uniforms.
            gl.uniform1f(u.iTime, performance.now())
            gl.uniform4f(u.iResolution, L.width, L.height, 0, 0)
            gl.uniform4f(u.iMouse, mouse.x * L.width, (1 - mouse.y) * L.height, mouse.main, mouse.aux)
            gl.uniform1f(u.iSlowdown, L.iSlowdown)
            gl.uniformMatrix3fv(u.iMixing, false, L.iMixing)
            gl.uniform3fv(u.iGrowthCenter, L.iGrowthCenter)
            gl.uniform3fv(u.iGrowthWidth, L.iGrowthWidth)
            gl.uniform2fv(u.iOffset, L.iOffset)
            gl.uniform2fv(u.iKernelOffset, L.iKernelOffset)

            // Compute the next frame.
            s.leniaFrames.prev.read(gl, 0, u.leniaGrid)
            s.leniaKernel.read(gl, 1, u.leniaKernel)
            s.leniaFrames.next.write(gl)

            // Draw the fullscreen rectangle.
            rect.draw(gl, a.vertexPos)
            // gl.clear(gl.COLOR_BUFFER_BIT) // TODO:

            s.leniaFrames.next.copyTo(gl, s.leniaFrames.extra)
            s.leniaFrames.next.resetWrite(gl)
        }
        if (p2 !== null) {
            const u = p2.uniform, a = p2.attrib
            gl.useProgram(p2.program)
            gl.uniform1f(u.iTime, performance.now())
            gl.uniform4f(u.iResolution, L.width, L.height, 0, 0)
            s.leniaFrames.next.read(gl, 0, u.leniaGrid)
            s.leniaFrames.extra.write(gl)
            s.posSpeed.prev.read(gl, a.posSpeed)
            s.posSpeed.next.write(gl, 0)
            s.extraState.prev.read(gl, a.extraState)
            s.extraState.next.write(gl, 1)
            gl.enable(gl.BLEND), gl.blendFunc(gl.ONE, gl.ONE)
            s.emitRadius.draw(gl, a.emitRadius, gl.POINTS, true)
            gl.disable(gl.BLEND), gl.blendFunc(gl.ONE, gl.ZERO)
            s.extraState.next.resetWrite(gl, 1)
            s.posSpeed.next.resetWrite(gl, 0)
            s.leniaFrames.extra.resetWrite(gl)
            swap(s.posSpeed), swap(s.extraState)
        }
        if (p3 !== null) { // Display what happened.
            const u = p3.uniform, a = p3.attrib
            gl.useProgram(p3.program)
            gl.uniform4f(u.iResolution, L.width, L.height, 0, 0)
            gl.uniform4f(u.iDisplay, gl.drawingBufferWidth, gl.drawingBufferHeight, 0, 0)
            s.leniaFrames.extra.read(gl, 0, u.leniaGrid)
            s.leniaKernel.read(gl, 1, u.leniaKernel)
            gl.uniformMatrix4fv(u.iColorMatrix, false, L.iColorMatrix)
            rect.draw(gl, a.vertexPos)
        }

        swap(s.leniaFrames, 'prev', 'extra')
        gl.flush()
    }
    function swap(a, k1='prev', k2='next') { [a[k1], a[k2]] = [a[k2], a[k1]] }
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

function initShaders(gl, [vsSource, fsSource], {uniforms, attribs, transformFeedback}) {
    // Compiles vertex+fragment shaders in a WebGL context.
    const vs = initShader(gl, gl.VERTEX_SHADER, vsSource)
    const fs = initShader(gl, gl.FRAGMENT_SHADER, fsSource)
    if (vs === null || fs === null) return null
    const program = gl.createProgram()
    gl.attachShader(program, vs)
    gl.attachShader(program, fs)
    if (transformFeedback)
        gl.transformFeedbackVaryings(program, transformFeedback, gl.SEPARATE_ATTRIBS)
    gl.linkProgram(program)
    gl.validateProgram(program)
    if (!gl.getProgramParameter(program, gl.LINK_STATUS)) {
        error(gl.getProgramInfoLog(program))
        gl.deleteProgram(program)
        return null
    }
    const r = { program, uniform:Object.create(null), attrib:Object.create(null) }
    if (uniforms)
        for (let u of uniforms)
            r.uniform[u] = gl.getUniformLocation(program, u) // null if not found.
    if (attribs)
        for (let u of attribs)
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
    //   Read with `r.read(gl, gl.getAttribLocation(program, 'attrib'))` as a vertex attribute.
    //   Draw this list of vertices with `r.draw(gl, gl.getAttribLocation(program, 'attrib'), gl.POINTS, false)`.
    //   Write with `r.write(gl, 0)` then `r.resetWrite(gl)` as transform-feedback of a vertex shader.
    //     If there are any buffer writes, pass `true` to `.draw`.
    if (!(f32 instanceof Float32Array)) f32 = Float32Array.from(f32)
    const buf = gl.createBuffer()
    gl.bindBuffer(gl.ARRAY_BUFFER, buf)
    gl.bufferData(gl.ARRAY_BUFFER, f32, usageHint)
    return {
        buf,
        length: f32.length,
        numbersPerValue,
        read(gl, attribLocation) {
            if (attribLocation < 0) throw new Error("Attrib not found")
            gl.bindBuffer(gl.ARRAY_BUFFER, this.buf)
            gl.enableVertexAttribArray(attribLocation)
            gl.vertexAttribPointer(attribLocation, this.numbersPerValue, gl.FLOAT, false, 0, 0)
        },
        draw(gl, attribLocation, mode = gl.TRIANGLE_STRIP, needsTransformFeedback = false) {
            this.read(gl, attribLocation)
            if (needsTransformFeedback) gl.beginTransformFeedback(gl.POINTS)
            gl.drawArrays(mode, 0, this.length / this.numbersPerValue | 0)
            if (needsTransformFeedback) gl.endTransformFeedback()
        },
        write(gl, index = 0) {
            gl.bindBufferBase(gl.TRANSFORM_FEEDBACK_BUFFER, index, this.buf)
        },
        resetWrite(gl, index) {
            gl.bindBufferBase(gl.TRANSFORM_FEEDBACK_BUFFER, index, null)
        },
    }
}

function initTexture(gl, width, height, pixels = null) {
    // A 2D array of RGBA values `r`.
    //   Read with `r.read(gl, 0, gl.getUniformLocation(program, 'textureName'))` in JS,
    //     `uniform sampler2D textureName;  void main(void) { texture2D(textureName, vec2(0., 0.)) }` in GLSL.
    //     (In-JS's index, `0` in this example, has no meaning except that it doesn't overlap with other texture reads.)
    //   Write with `r.write(gl)`, then finally `r.resetWrite(gl)`.
    //     (Can only write to 1 texture at a time.)
    //     Reset with `gl.bindFramebuffer(gl.FRAMEBUFFER, null), gl.viewport(0,0, gl.canvas.width, gl.canvas.height)`.
    //   To write a copy to another place too, use `r.copyTo(gl, r2)`.
    const tex = gl.createTexture()
    gl.bindTexture(gl.TEXTURE_2D, tex)
    gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, width, height, 0, gl.RGBA, gl.UNSIGNED_BYTE, pixels)
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR)

    return { // Who needs JS classes when you have *objects*? What are we aiming for anyway, *efficiency* or something?
        R: tex,
        W: null,
        _width: width,
        _height: height,
        _cache: Object.create(null),
        read(gl, i, uniformLocation) {
            gl.activeTexture(this._cache[i] || (this._cache[i] = gl['TEXTURE'+i]))
            gl.bindTexture(gl.TEXTURE_2D, this.R)
            gl.uniform1i(uniformLocation, i)
        },
        write(gl) {
            if (!this.W) {
                gl.bindFramebuffer(gl.FRAMEBUFFER, this.W = gl.createFramebuffer())
                gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_2D, tex, 0)
            } else gl.bindFramebuffer(gl.FRAMEBUFFER, this.W)
            gl.viewport(0,0, this._width, this._height)
        },
        copyTo(gl, texture) {
            gl.bindTexture(gl.TEXTURE_2D, texture.R)
            gl.copyTexImage2D(gl.TEXTURE_2D, 0, gl.RGBA, 0, 0, this._width, this._height, 0)
        },
        resetWrite(gl) {
            gl.bindFramebuffer(gl.FRAMEBUFFER, null)
            gl.viewport(0,0, gl.canvas.width, gl.canvas.height)
        },
    }
}

function error(...msg) { console.error(...msg) }

function loadLevel(url) {
    // Returns a promise of a level's object.
    return fetch(url, {mode:'cors'}).then(response => response.json())
}

function leniaKernel(gl, R, mus, sigmas, offsets, data=null, totals=[0,0,0,1]) {
    // Pre-computes a Lenia kernel, with 3 colors.
    // The `result` can be given to `initTexture(gl, 2*R+1, 2*R+1, pixels)`.
    const sz = 2*R+1, colors = 3
    if (!data) {
        data = new Float32Array(sz*sz * 4) // Pre-normalization.
        const [dx, dy] = offsets
        for (let y = -R; y <= R; ++y)
            for (let x = -R; x <= R; ++x) {
                const r = Math.hypot(x - dx, y - dy) / R // 0..sqrt(2), usually.
                for (let c=0; c < colors; ++c) {
                    const weight = bell(r, mus[c], sigmas[c])
                    const index = (y+R) * sz + (x+R)
                    data[4*index + c] = weight
                    totals[c] = Math.max(totals[c], weight)
                }
            }
    }
    const data2 = new Uint8Array(sz*sz * 4) // Normalized, so that max is 1.
    for (let y = -R; y <= R; ++y)
        for (let x = -R; x <= R; ++x) {
            const index = (y+R) * sz + (x+R)
            for (let c=0; c < 4; ++c) {
                data2[4*index + c] = c < colors ? Math.round(data[4*index + c] / totals[c] * 255) : 255
            }
        }
    const r = initTexture(gl, sz, sz, data2)
    return r
    function bell(x, m, s) { return Math.exp(-(x-m)*(x-m)/(s*s*2.)) }
}