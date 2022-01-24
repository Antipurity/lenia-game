// Do `localStorage.debug=true` in JS console to be able to reload levels that you are changing.

// ...I keep encountering those cool animations. Too bad I don't have a means to make them into DOM elements, for portraits in story mode.

// TODO: ...With the means to make a story and go across levels, come up with concrete levels.
// Levels, the meat of the game, allowing dynamic discoveries of whole different worlds of complexity.
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
// TODO: ...Are all the levels above worthless? ...Eye-of-the-storm (menu's level) seemed accurate.
// Glider-shooting (kinda hard, but possible, and gliders neither widen nor shrink): level.iMixing = [-0.14, -0.28, 0.95, -0.15, 0.47, -2, -1.03, 0.43, 0.54]
// Green/red glider shooting: {"iMixing":[1.05,-3,0.95,0.246,0.47,-2,-3,0.43,0.54],"kernel":{"center":[0.5,0.5,0.5],"width":[0.14,0.14,0.14]},"iGrowthCenter":[0.4,0.2,0.4],"iGrowthWidth":[0.08,0.142,0.1]}
//   Few primary green gliders: "pointer": {"displayRadius":[0,8,0], "pos":[0.5, 0.25], "emitRadius":8, "emit":"green", "emittance":1, "speed":{"Bmouse":0.01, "Bspeed": 0.93}}
//   Many secondary red gliders: "pointer": {"displayRadius":[0,8,0], "pos":[0.5, 0.25], "emitRadius":8, "emit":"green", "emittance":1, "speed":{"Bmouse":0.01, "Bspeed": 0.93}}
//   Could be fun trying to create green waves to hit enemies without creating red waves to hit friends. Especially with a multi-pointer chain.
//   Attractions:
//     G stdev = .142: pointer movement creates bizarre smooth repeating trails. Like pretty magic barriers. (.143 makes these trails never go away.)
// Blue strings with a cybernetic texture when in blobs: {"iMixing":[0.5,0.58,0,-3,0.42,0.49,0,0.568,0.86],"kernel":{"center":[0.5,0.5,0.5],"width":[0.1,0.1,0.1]},"iGrowthCenter":[0.5,0.5,0.5],"iGrowthWidth":[0.1,0.1,0.1]}
// Beautiful fireballs (emittance 3) with almost no waste, sometimes the bombs left behind explode colorfully, but rarely the byproducts decide to replicate and fill the whole screen: {"iMixing":[0.5,0.58,0.23,-3,0.42,0.49,-0.816,0.568,0.855],"kernel":{"center":[0.5,0.5,0.5],"width":[0.1,0.1,0.1]},"iGrowthCenter":[0.5,0.5,0.5],"iGrowthWidth":[0.1,0.1,0.15]}
//   Attractions:
//     Much more frequent and better-looking infestations: {"iMixing":[0.5,0.58,0.23,3,0.42,0.49,-0.816,0.754,0.798],"kernel":{"center":[0.5,0.5,0.5],"width":[0.1,0.1,0.1]},"iGrowthCenter":[0.5,0.5,0.5],"iGrowthWidth":[0.1,0.1,0.15]}
// Slowly-dissipating thorny trails: {"iMixing":[-2.513,1.083,0.23,1.974,0.176,0.402,-0.816,-1.509,0.756],"kernel":{"center":[0.5,0.5,0.5],"width":[0.1,0.1,0.1]},"iGrowthCenter":[0.5,0.5,0.5],"iGrowthWidth":[0.176,0.1,0.15]}
// Weird road construction simulator: {"iMixing":[0.883,-0.575,-0.774,0.12,-0.376,0.615,0.16,0.572,-0.051],"kernel":{"center":[0.8,0.5,0.5],"width":[0.01,0.1,0.1]},"iGrowthCenter":[0.5,0.5,0.5],"iGrowthWidth":[0.6,0.2,0.2]}
// Leisurely swirly maze (eye of the storm, but the eyes don't ever actually change location): {"iMixing":[0.756,-0.575,-0.774,0.12,-0.376,0.615,0.16,-0.762,0.968],"kernel":{"center":[0.502,0.5,0.5],"width":[0.09,0.1,0.1]},"iGrowthCenter":[0.5,0.5,0.5],"iGrowthWidth":[0.303,0.2,0.2]}
// Planting the infection: {"iMixing":[3,0.062,3,2.356,2.384,-1.915,1.308,3,0.161],"kernel":{"center":[0.5,0.5,0.5],"width":[0.1,0.1,0.1]},"iGrowthCenter":[0.757,0.658,0.8],"iGrowthWidth":[0.048,0.123,0.09]}
// Solid-coloring game (green stays, blue fades, red only stays if not small; green blocks red, but blue easily paints over green, and blue encourages red to follow in strings, so circling red with blue can fill the level — need JS to switch sides in one level to keep it interesting): {"iMixing":[1.183,-3,0.416,-0.023,0.685,-0.745,0.076,1.079,2.087],"kernel":{"center":[0.5,0.5,0.5],"width":[0.1,0.1,0.1]},"iGrowthCenter":[0.757,0.658,2.414],"iGrowthWidth":[0.176,0.289,1.025]}
//   Attractions:
//     Blue eats at green with pretty patterns at entry/exit points, stays much longer, and is eaten by red: {"iMixing":[1.183,-1.701,0.416,0.275,0.685,-0.745,-0.433,1.079,2.087],"kernel":{"center":[0.5,0.5,0.5],"width":[0.1,0.1,0.1]},"iGrowthCenter":[0.757,0.701,3],"iGrowthWidth":[0.176,0.332,1.572]}
// Red wants-to-be-a-square tumor (its inside can become staticy), blue holey painting, and when they intersect, they both get infected with green maggots: {"iMixing":[1.261,-1.701,0.416,0.36,3,0.529,-0.433,-1.043,1.025],"kernel":{"center":[0.5,0.5,0.5],"width":[0.2,0.1,0.1]},"iGrowthCenter":[0.757,0.701,0.757],"iGrowthWidth":[0.176,0.077,0.218]}
//   Attractions:
//     Green eats at blue but not at red: {"iMixing":[1.261,-1.701,0.416,0.36,1.833,0.529,-0.433,-1.043,1.025],"kernel":{"center":[0.5,0.5,0.5],"width":[0.2,0.1,0.1]},"iGrowthCenter":[0.757,0.701,0.757],"iGrowthWidth":[0.176,0.077,0.218]}
//     Contact is usually more damaging to red, but interactions are complex: {"iMixing":[1.261,-1.701,0.416,0.505,1.833,0.275,-0.433,-1.043,1.025],"kernel":{"center":[0.5,0.5,0.5],"width":[0.2,0.1,0.1]},"iGrowthCenter":[0.757,0.701,0.757],"iGrowthWidth":[0.176,0.077,0.218]}
//     Interactions are much more damaging to red: {"iMixing":[1.261,-1.701,0.416,0.55,1.833,0.275,-0.433,-1.043,1.025],"kernel":{"center":[0.5,0.5,0.5],"width":[0.2,0.1,0.1]},"iGrowthCenter":[0.757,0.701,0.757],"iGrowthWidth":[0.176,0.077,0.218]}

// (Guess Lenia is more boring than I thought.)

// TODO: Have a README.md.
    // TODO: Make note of browser compatibility, according to the APIs that we use: WebGL2, Object.values, object destructuring, element.append(…), pointer events.
// TODO: With a direct-link library, expose surroundings (the display, since just reading from a texture is quite hard) & {x,y,data:[targetX, targetY, health, emitColor]} per-actor position (target is mouse if no target) of all agents with `displayRadius`, into sound. This might be the coolest application that I can think of: controlling a swarm.
//   ...How, exactly? I guess, on level load, we want to (pause previous sensors and) create a sensor for each actor with `displayRadius`...
// TODO: A license notice in this file.



;(function loop(canvas, exports) {
    const R = 5 // Convolution-kernel radius. 2*R+1 is the kernel resolution.
    const initialLevel = 'levels/initial.json'



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
    for (int y=-R; y<=R; ++y)
    for (int x=-R; x<=R; ++x) { // Convolution with a pre-computed kernel.
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

    gl_FragColor = vec4(rgb, 1.);
}`



    const displayLeniaSource = `
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

uniform float iTime;
uniform vec4 iMouse;
uniform vec4 iResolution;
uniform sampler2D leniaGrid;

// 13 attributes.
attribute vec4 posSpeed; // x/y/dx/dy
attribute vec4 extraState; // health/score/emitRadius/emitColor (emitColor is 0/1/2 for r/g/b)
attribute vec4 gravity; // gravityX/gravityY/targetX/targetY
// Behavior matrix; each attribute is the input's contribution to each output, which is speed/emittance/dhealth/dscore.
//   For positions/velocities/color-differentials, x and y use the same weights for compactness.
attribute vec4 B1;
attribute vec4 Bspeed;
attribute vec4 Bmouse;
attribute vec4 Btarget;
attribute vec4 Bhealth;
attribute vec4 Br;
attribute vec4 Bg;
attribute vec4 Bb;
attribute vec4 Btime;
attribute vec4 BtimeFrequency; // In 1/seconds.

varying vec4 outPosSpeed; // x/y/dx/dy
varying vec4 outExtraState; // health/score/emitRadius/emitColor
varying vec4 emit;

const int R = ${R};
const float TAU = 2. * 3.14159265359;

vec2 closestWrapOffset(vec2 a, vec2 b) { // An approximation of it, anyway.
    return vec2(a.x < .1 && b.x > .9 ? 1. : b.x < .1 && a.x > .9 ? -1. : 0., a.y < .1 && b.y > .9 ? 1. : b.y < .1 && a.y > .9 ? -1. : 0.);
}

void main() {
    // Read near-to-actor colors and color-edges from Lenia state, to give them as inputs to behavior, 0..1.
    vec2 at = posSpeed.xy;
    vec3 near = vec3(0., 0., 0.);
    vec3 nearDX = vec3(0., 0., 0.);
    vec3 nearDY = vec3(0., 0., 0.);
    float maxDiff = 0.;
    for (int y=-R; y<=R; ++y)
    for (int x=-R; x<=R; ++x) {
        vec2 txy = mod(at + vec2(x,y) / iResolution.xy, 1.);
        vec3 color = texture2D(leniaGrid, txy).rgb;
        near += color;
        if (x != 0) nearDX += sign(float(x)) * color / abs(float(x));
        if (y != 0) nearDY += sign(float(y)) * color / abs(float(y));
        if (x > 0) maxDiff += 1. / abs(float(x));
    }
    near /= float(${(2*R+1) * (2*R+1)});
    nearDX /= maxDiff;
    nearDY /= maxDiff;

    vec2 mouseVec = iMouse.xy / iResolution.xy;
    vec2 targetVec = gravity.zw;
    mouseVec -= at + closestWrapOffset(at, mouseVec);
    targetVec -= at + closestWrapOffset(at, targetVec);
    float health = extraState.x;
    vec2 speed = posSpeed.zw;

    // Update position & state.
    vec2 dPos = gravity.xy + B1.x + speed*Bspeed.x + mouseVec*Bmouse.x + targetVec*Btarget.x + health*Bhealth.x + vec2(nearDX.r, nearDY.r)*Br.x + vec2(nearDX.g, nearDY.g)*Bg.x + vec2(nearDX.b, nearDY.b)*Bb.x + sin(BtimeFrequency.x * iTime*TAU) * Btime.x;
    vec2 nextPos = mod(at + dPos, 1.);
    outPosSpeed = vec4(nextPos, dPos);

    float speedL = length(speed), mouseVecL = length(mouseVec), targetVecL = length(targetVec);

    if (extraState.x > 0.) {
        float dhealth = B1.z + speedL*Bspeed.z + mouseVecL*Bmouse.z + targetVecL*Btarget.z + health*Bhealth.z + near.r*Br.z + near.g*Bg.z + near.b*Bb.z + sin(BtimeFrequency.z * iTime*TAU) * Btime.z;
        float dscore  = B1.w + speedL*Bspeed.w + mouseVecL*Bmouse.w + targetVecL*Btarget.w + health*Bhealth.w + near.r*Br.w + near.g*Bg.w + near.b*Bb.w + sin(BtimeFrequency.w * iTime*TAU) * Btime.w;
        outExtraState = vec4(clamp(extraState.x + dhealth, 0., 1.), extraState.y + dscore, extraState.zw);
    } else
        outExtraState = vec4(0., extraState.y, extraState.zw);

    float emitRadius = extraState.z;
    vec3 color = extraState.w<.5 ? vec3(1.,0.,0.) : extraState.w<1.5 ? vec3(0.,1.,0.) : vec3(0.,0.,1.);
    float emittance = B1.y + speedL*Bspeed.y + mouseVecL*Bmouse.y + targetVecL*Btarget.y + health*Bhealth.y + near.r*Br.y + near.g*Bg.y + near.b*Bb.y + sin(BtimeFrequency.y * iTime*TAU) * Btime.y;
    emit = vec4(emittance * color, emitRadius);

    gl_Position = vec4(outPosSpeed.xy * 2. - 1., 0., 1.);
    gl_PointSize = emitRadius;
}

=====

precision highp float;

uniform vec4 iResolution;
uniform sampler2D leniaGrid;

varying vec4 outPosSpeed; // x/y/dx/dy
varying vec4 emit;

void main() {
    vec2 center = outPosSpeed.xy;
    float distance = length(gl_FragCoord.xy - center * iResolution.xy) / emit.w * 2.;
    if (distance < 1.) // Relies on blending.
        gl_FragColor = vec4(emit.rgb, 1.) * (1. - distance);
    else
        discard;
}
`



    const displayActorsSource = `
precision highp float;

uniform vec4 iDisplay;

attribute vec4 posSpeed; // x/y/dx/dy
attribute vec4 extraState; // health/score/emitRadius/emitColor
attribute vec4 displayRadius; // R/G/B/_

varying float health;
varying vec2 center;
varying vec4 emitColor;
varying vec4 emitRadius;

void main() {
    emitRadius = displayRadius;
    emitColor = vec4(0.,0.,0.,0.);
    if (displayRadius.r > 0.) emitColor += vec4(1.,0.,0.,0.);
    if (displayRadius.g > 0.) emitColor += vec4(0.,1.,0.,0.);
    if (displayRadius.b > 0.) emitColor += vec4(0.,0.,1.,0.);

    center = posSpeed.xy;
    health = extraState.x;
    gl_Position = vec4(center * 2. - 1., 0., 1.);
    gl_PointSize = max(emitRadius.r, max(emitRadius.g, emitRadius.b)) * iDisplay.z;
}

=====

precision highp float;

uniform float iTime;
uniform vec4 iDisplay;
uniform sampler2D leniaGrid;
uniform mat4 iColorMatrix;

varying float health;
varying vec2 center;
varying vec4 emitColor;
varying vec4 emitRadius;

void main() {
    vec4 distances = length(gl_FragCoord.xy - center * iDisplay.xy) / (emitRadius + 1.) * 2. / iDisplay.z;
    float minD = min(distances.r, min(distances.g, distances.b));
    if (minD < 1.) {
        vec4 inner = sign((.6 - distances) + abs(.6 - distances));
        vec4 outer = sign((1. - distances) + abs(1. - distances));
        gl_FragColor = (emitColor * (1. - distances / .6) + (1. - inner) * outer * vec4(emitColor.rgb, 1.) * health) * iColorMatrix;
    } else discard;
}
`



    const mouse = { x:.5, y:.5, main:false, aux:false, update(evt) {
        mouse.x = ((evt.changedTouches ? evt.changedTouches[0] : evt).clientX + (evt.movementX || 0)) / innerWidth
        mouse.y = ((evt.changedTouches ? evt.changedTouches[0] : evt).clientY + (evt.movementY || 0)) / innerHeight
        mouse.main = evt.buttons & 1
        mouse.aux = evt.buttons & 2
    } }
    addEventListener('pointerdown', mouse.update, {passive:true})
    addEventListener('touchmove', mouse.update, {passive:true})
    addEventListener('pointermove', mouse.update, {passive:true})
    addEventListener('pointerup', mouse.update, {passive:true})



    // For actors' JS.
    const api = exports.api = {
        _level: null, _url: null, _windowShorteners: new Set,
        _soundHandler: null,
        levelLoad(url = api._url) {
            // Goes to a level. `url` must point to a JSON file of the level.
            if (api._level && !api._level.isMenu) api.levelSuggest(api._level.url, { lost:api._level.score })
            api._url = url
            loadLevel(url).then(L => {
                L.url = url
                api._level = L
                api.window(null)
                glState.leniaFrames = null
                if (L.isMenu) storeSet('menu', url)
                else {
                    api.levelSuggest(url)
                    api.levelSuggest().then(({won, lost}) => { L._won = won[url], L._lost = lost[url] })
                }
                exports.level = L
            }).catch(e => (error(e), api.levelLoad(initialLevel)))
        },
        levelSuggest(url, winLose = { lost:0 }) {
            // Given `url`, remembers it, to recommend to the user later. 🌟
            // Given `url` and `{ won:L.frame, lost:L.score }`, may update the min stored time.
            // Given `url` and `{ lost:L.score }`, such as on level lose or end, may update the max stored score.
            // Given nothing, fetches `{ won:{url:time}, lost:{url:score} }` for all URLs. Won levels are in both, non-won ones are in `lost`.
            // Given `url` and `{}`, forgets the level's data.
            const prev = api._levelSuggestLock // No data races.
            return api._levelSuggestLock = new Promise(((resolve, reject) => {
                Promise.resolve(prev).then(() => {
                    const novel = storeGet('novel')
                    return storeGet('won').then(won => novel.then(novel => {
                        won = won && JSON.parse(won) || {}
                        novel = novel && JSON.parse(novel) || {}
                        if (!url) return resolve({ won, lost:novel })
                        const waitFor = []
                        if (typeof winLose.won == 'number') { // Won a level. Remember min time.
                            won[url] = won[url] !== undefined ? Math.min(won[url], winLose.won) : winLose.won, waitFor.push(storeSet('won', JSON.stringify(won)))
                        }
                        if (typeof winLose.lost == 'number') { // Lost a level. Remember max score.
                            novel[url] = Math.max(novel[url] || 0, winLose.lost), waitFor.push(storeSet('novel', JSON.stringify(novel)))
                        }
                        if (typeof winLose.won != 'number' && typeof winLose.lost != 'number') { // Forget about Freeman.
                            delete novel[url], waitFor.push(storeSet('novel', JSON.stringify(novel)))
                            delete won[url], waitFor.push(storeSet('won', JSON.stringify(won)))
                        }
                        Promise.all(waitFor).then(resolve).catch(reject)
                    })).catch(reject)
                }).catch(reject)
            }))
        },
        levelExit() {
            // Returns to the last-visited main menu.
            if (api._level && !api._level.isMenu) api.levelSuggest(api._level.url, { lost:api._level.score })
            storeGet('menu').then(url => api.levelLoad(url || initialLevel))
        },
        read(actorName) {
            // Syncs GPU state to our CPU actor object, namely, position+speed. After this, writing can proceed safely.
            // Can be called slow.
            const L = api._level, a = L.actors[actorName]
            if (!a) throw new Error("Nonexistent actor "+actorName)
            if (!glState.leniaFrames) return
            updateActorCPUData(L, 1, a.i)
        },
        write(actorName) {
            // After changing an actor object's props, call this to sync changes to GPU.
            const L = api._level, a = L.actors[actorName]
            if (actorName != null) { // Update an actor.
                if (!a) throw new Error("Nonexistent actor "+actorName)
                updateActor(L, a)
                updateActorWebGLData(L, a.i)
            } else // Update the level.
                updateLevelWebGLData(L)
        },
        window(content, actorName = null, timeoutSec = 16, posMomentum = .98) {
            // Given a string or a DOM element or an array tree, and the actor name, positions a window that follows the actor.
            // Given a string or a DOM element or an array tree, positions a free-floating window in the bottom-left corner.
            // To not fade away after `timeoutSec`, pass `timeoutSec = null`.
            // Given nothing, clears every window instantly. (Level load does this.)
            // Returns a promise, which resolves when the timeout has passed.
            if (!document.body) return
            return Promise.resolve(content).then(content => {
                if (content == null) {
                    for (let el of document.querySelectorAll('.window')) {
                        el.classList.add('removed')
                        el.remove()
                    }
                    return
                }
                if (typeof content == 'string') { const el = document.createElement('div');  el.append(content);  content = el }
                if (Array.isArray(content)) { // Ex: [{ tag:'div', style:'color:red', onclick() { api.levelLoad() } }, 'Click to reload the level']
                    content = (function arrayTreeToDOM(x) {
                        if (x instanceof Promise) {
                            const el = document.createElement('div')
                            x.then(x => el.replaceWith(arrayTreeToDOM(x)), err => el.replaceWith(err instanceof Error ? '<Error: '+err.message+'>' : '<Error>'))
                            el.classList.add('promise')
                            return el
                        } else if (Array.isArray(x)) {
                            let tag = 'span'
                            for (let i = 0; i < x.length; ++i) if (x[i] && !(x[i] instanceof Node) && typeof x[i].tag == 'string') tag = x[i].tag
                            const el = document.createElement(tag)
                            for (let i = 0; i < x.length; ++i)
                                if (x[i] && !Array.isArray(x[i]) && typeof x[i] == 'object' && !(x[i] instanceof Promise) && !(x[i] instanceof Node))
                                    for (let k of Object.keys(x[i])) {
                                        const v = el[k] = x[i][k]
                                        if (k !== 'tag' && (typeof v == 'string' || typeof v == 'number' || typeof v == 'boolean'))
                                            el.setAttribute(k, v)
                                    }
                                else if (x[i] != null) el.append(arrayTreeToDOM(x[i]))
                            return el
                        } else if (x instanceof Node) return x
                        else return document.createTextNode(''+x)
                    })(content)
                }
                content.classList.add('window')
                if (actorName && api._level.actors[actorName]) {
                    const margin = 6 // Personal space, buddy.
                    let actor = api._level.actors[actorName], x, y
                    moveWindow()
                    function moveWindow() { // Reasonable amount of code for tracking actors.
                        if (!api._level.actors[actorName] || actor !== api._level.actors[actorName]) return
                        if (!content.classList.contains('removed')) requestAnimationFrame(moveWindow)
                        if (document.visibilityState === 'hidden') return
                        const p = posMomentum
                        const [x2, y2] = actor.pos
                        const w = innerWidth, h = innerHeight, m = margin
                        const width = content.offsetWidth / w, height = content.offsetHeight / h
                        if (p !== 0) { // Mosey along.
                            const boxX = content.offsetLeft / w, boxY = 1 - content.offsetTop / h
                            const distX = Math.min(Math.hypot(x2-boxX+m/w, y2-boxY+height/2), Math.hypot(x2-boxX-width-m/w, y2-boxY+height/2))
                            const distY = Math.min(Math.hypot(y2-boxY-m/h, x2-boxX-width/2), Math.hypot(y2-boxY+height+m/h, x2-boxX-width/2))
                            const [leftX, rightX, topY, bottomY] = distX < distY ? [width/2, width/2, -m/h, height + m/h] : [-m/w, width + m/w, height/2, height/2]
                            const x3 = x == null ? x2 : x2-leftX < x ? x2 + m/w : x2-rightX > x ? x2-width - m/w : x
                            const y3 = y == null ? y2 : y2-topY < y ? y2 + m/h : y2-bottomY > y ? y2-height - m/h : y
                            x = x != null ? p*x + (1-p)*x3 : x3 - Math.random()*width
                            y = y != null ? p*y + (1-p)*y3 : y3 - Math.random()*height
                        } else { // Instantly center.
                            x = x2 - width/2, y = y2 - height/2
                        }
                        x = Math.max(0, Math.min(x, 1-width))
                        y = Math.max(0, Math.min(y, 1-height))
                        content.style.left = x*w + 'px'
                        content.style.top = (1-y - height)*h + 'px'
                    }
                } else // Bottom-left when no actor or a non-existent actor is given.
                    content.style.left = content.style.bottom = 0
                document.body.append(content)
                return new Promise((resolve, reject) => {
                    if (timeoutSec != null) {
                        const start = performance.now()
                        let duration = timeoutSec*1000, timeout = setTimeout(disappear, duration)
                        api._windowShorteners.add(content._windowShortener = sec => { // On click, take 2 seconds less to disappear.
                            clearTimeout(timeout)
                            duration -= sec*1000
                            timeout = setTimeout(disappear, duration - (performance.now() - start))
                        })
                        function disappear() {
                            api._windowShorteners.delete(content._windowShortener)
                            content.classList.contains('removed') ? reject('windows were cleared, do not proceed') : resolve('proceed')
                            content.classList.add('removed')
                            setTimeout(() => content.remove(), 5000)
                        }
                    } else resolve()
                })
            })
        },
        levelSelection(reportNovel = false) {
            // Given nothing, returns (a promise of) the DOM element for level-selection, suitable for `api.window`'s `content`.
            // Given `true`, returns (a promise of) how many levels are not-won (are novel).
            return api.levelSuggest().then(({ won, lost }) => {
                if (reportNovel) {
                    won = new Set(Object.keys(won))
                    return Object.keys(lost).filter(url => !won.has(url)).length
                }
                // Create a hierarchy containing `[url, wonFrame, lostScore]`.
                const data = Object.create(null)
                for (let k in lost) data[k] = [k, 0, lost[k]]
                for (let k in won) data[k] = [k, won[k], data[k] ? data[k][2] : 0]
                const tree = urlsToHierarchy(data)
                return toUI(computeChildSummaries(tree))
                function computeChildSummaries(x) {
                    // With this, parents display are-there-lost-children and sum-of-best-times and sum-of-best-scores.
                    if (Array.isArray(x)) return x
                    let novel = 0, wonSum = 0, lostSum = 0
                    for (let k in x) {
                        let v = computeChildSummaries(x[k])
                        if (!Array.isArray(v)) {
                            v = v.__summary
                            v[0] && (novel = true), wonSum += v[1], lostSum += v[2]
                        } else
                            v[1] === 0 && (novel = true), wonSum += v[1], lostSum += v[2]
                    }
                    x.__summary = [novel, wonSum, lostSum]
                    return x
                }
                function toUI(x, depth=0) {
                    if (Array.isArray(x)) { // A concrete level. Show description and an invitation.
                        const [url, wonFrame, lostScore] = x
                        return [{tag:'div'},
                            wonFrame !== 0 ? { style:'height:0px' } : null, // Only uncollapse novel levels.
                            { class:'hidable' },
                            [{tag:'div'},
                                { style:'float:right; clear:right; display:inline-block; text-align:right; font-size:.9em' },
                                [{ tag:'button', url, onclick() { api.levelLoad(this.url) } }, 'Go'],
                            ],
                            fetch(url, { mode:'cors', cache:'force-cache' }).then(r => r.json()).then(level => {
                                return [{tag:'div'},
                                    typeof level.description == 'string' ? level.description : 'No description. Cringe.',
                                ]
                            }),
                            [{tag:'div'}, { style:'clear:both' }],
                        ]
                    } else { // A parent node.
                        const children = []
                        const keys = Object.keys(x).filter(k => k !== '__summary').sort((a,b) => {
                            // Sort keys in lexicographic order, but ensure that `9 < 10`.
                            return a.replace(/[0-9]+/g, s => String.fromCodePoint(+s)).localeCompare(b.replace(/[0-9]+/g, s => String.fromCodePoint(+s)))
                        })
                        for (let k of keys) {
                            const [novel, wonFrame, lostScore] = Array.isArray(x[k]) ? [x[k][1] === 0, x[k][1], x[k][2]] : x[k].__summary
                            children.push([{tag:'div'},
                                { class: 'level-container' },
                                [{tag:'div'},
                                    novel ? { class:'novel-header' } : null,
                                    { onclick() { // Un/collapse the contents on click.
                                        const el = this.nextSibling, shown = !isHidden(el)
                                        if (shown) updateHeight(el)
                                        toggleHeight(el), el.style.setProperty('transition', 'none')
                                        if (!shown) updateHeight(el)
                                        toggleHeight(el), el.style.setProperty('transition', 'none')
                                        el.offsetHeight // Slow. But smooth.
                                        el.style.removeProperty('transition')
                                        toggleHeight(el)
                                        function isHidden(el) {
                                            return el.style.height == '0px'
                                        }
                                        function toggleHeight(el) {
                                            el.style.height = isHidden(el) ? el._height + 'px' : '0px'
                                        }
                                        function updateHeight(el, isParent = false) {
                                            if (!el || !el.style || el === document.body) return
                                            updateHeight(el.parentNode, true)
                                            if (isParent && !el._height) return
                                            el.style.height = 'auto'
                                            el._height = el.offsetHeight
                                        }
                                    } },
                                    [{ style:'vertical-align:middle; line-height:1.7em' }, k],
                                    [{tag:'div'},
                                        { style:'float:right; clear:right; display:inline-block; text-align:left; font-size:.75em; margin-left:1.2em; line-height:1.1em' },
                                        [{tag:'div'}, 'Score  ', [{ class:'numeric-information' }, lostScore !== 0 ? lostScore.toFixed(2) : '—']],
                                        [{tag:'div'}, 'Time   ', [{ class:'numeric-information' }, wonFrame !== 0 ? (wonFrame/60).toFixed(2) + 's' : '—']],
                                    ],
                                    [{ tag:'div', style:'clear:both' }],
                                ],
                                toUI(x[k], depth+1),
                            ])
                        }
                        return [
                            { tag:'div', class:'hidable' },
                            depth ? { style:['padding-left:1em', x.__summary[0] && depth > 1 ? '' : 'height:0px'].join(';') } : null,
                            ...children,
                        ]
                    }
                }
            })
        },
        share(url = api._url) { // Returns a URL that would open this level's URL in the site when opened.
            const u = new URL(location)
            u.hash = encodeURIComponent(url)
            return ''+u
        },
        _windowsAreShorterNow(bySeconds) {
            if (typeof bySeconds != 'number') bySeconds = 4
            api._windowShorteners.forEach(f => f(bySeconds))
        },
    }
    addEventListener('pointerdown', api._windowsAreShorterNow, {passive:true})
    // The main drawing loop.
    if (!canvas.gl)
        canvas.gl = canvas.getContext('webgl2', {alpha:false, desynchronized:true})
    const gl = canvas.gl
    if (!gl) throw new Error("Failed to get a WebGL2 context.")
    canvas.width = canvas.height = 0

    const glState = {
        // Shader programs.
        lenia: null,
        actors: null,
        displayLenia: null,
        displayActors: null,
        // Buffers.
        posBuffer: null, // For vertices of the full-screen quad.
        posSpeed: null, // Actor x/y/dx/dy.
        extraState: null, // Actor health/score/emitRadius/dummy.
        gravity: null, // Actor gravityX/gravityY/_/_.
        displayRadius: null, // R/G/B/_.
        behavior: null, // An object containing per-actor behavior matrices.
        // Textures.
        leniaFrames: null, // {prev, next}
        leniaKernel: null,
    }
    const Boutputs = ['speed', 'emittance', 'dhealth', 'dscore'], empty = Object.create(null)

    if (!location.hash)
        api.levelExit()
    else
        api.levelLoad(decodeURIComponent(location.hash).slice(1))
    setup()
    draw()

    canvas.addEventListener('webglcontextlost', evt => { evt.preventDefault(), api._level && (api._level._webglLost = true) }) // Allow restoring.
    canvas.addEventListener('webglcontextrestored', setup)
    function setup() {
        api.window(null)
        const s = glState
        s.lenia = initShaders(gl, leniaSource.split('====='), { uniforms:[
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
            'iMouse',
            'iResolution',
            'leniaGrid',
        ], attribs:[
            'posSpeed',
            'extraState',
            'gravity',
            'B1',
            'Bspeed',
            'Bmouse',
            'Btarget',
            'Bhealth',
            'Br',
            'Bg',
            'Bb',
            'Btime',
            'BtimeFrequency',
        ], transformFeedback:[
            'outPosSpeed',
            'outExtraState',
        ] })
        s.displayLenia = initShaders(gl, displayLeniaSource.split('====='), { uniforms:[
            'iColorMatrix',
            'iDisplay',
            'iResolution',
            'leniaGrid',
            'leniaKernel',
        ], attribs:[
            'vertexPos',
        ]})
        s.displayActors = initShaders(gl, displayActorsSource.split('====='), { uniforms:[
            'iColorMatrix',
            'iTime',
            'iDisplay',
            'leniaGrid',
        ], attribs:[
            'posSpeed',
            'extraState',
            'displayRadius',
        ]})
        s.posBuffer = initBuffer(gl, new Float32Array([-1,1, 1,1, -1,-1, 1,-1]), 2)
        gl.clearColor(0,0,0,1)
        api._level && (api._level._webglLost = false)
        s.leniaFrames = null
    }
    function handleLevelLoaded(s, L) {
        // This is separated from `setup` and called in `draw` after init, because it depends on the level `L`.
        // This might leak memory, since we don't manually dispose anything.
        if (s.leniaFrames) return
        L._webglLost = false
        s.leniaFrames = {
            prev:initTexture(gl, L.width, L.height),
            next:initTexture(gl, L.width, L.height), // The Lenia loop modifies Lenia state.
            extra:initTexture(gl, L.width, L.height), // Actors modify Lenia state too.
        }
        updateLevelWebGLData(L)

        try {
            if (typeof L.onLoad == 'string') L.onLoad = new Function('api,level', L.onLoad)
            if (typeof L.onLoad == 'function') L.onLoad(api, L)
        } catch (err) { console.error(err) }

        // Load actors.
        const actors = L.actors
        L.score = L.score || 0, L.winScore = typeof L.winScore == 'number' ? L.winScore : 1, L.frame = 0, L._trackedLost = 0
        L._actorNames = actors ? Object.keys(actors) : []
        const pos = b() // x/y/dx/dy
        const extra = b() // health/score/emitRadius/emitColor
        const gravity = b()
        const displayRadius = b()
        const B = { B1:b(), Bspeed:b(), Bmouse:b(), Btarget:b(), Bhealth:b(), Br:b(), Bg:b(), Bb:b(), Btime:b(), BtimeFrequency:b() }
        B.keys = Object.keys(B)
        L._buffers = { pos, extra, gravity, displayRadius, B }
        let i = 0
        for (let name of L._actorNames)
            actors[name].i = i++
        for (let name of L._actorNames) {
            const a = actors[name]
            updateActor(L, a)
            try {
                if (typeof a.onLoad == 'string') a.onLoad = new Function('api,level,actorName', a.onLoad)
                if (typeof a.onLoad == 'function') a.onLoad(api, L, name)
            } catch (err) { console.error(err) }
        }
        if (!L._trackedLost) L._trackedLost = null
        s.posSpeed = twice(() => initBuffer(gl, pos, 4))
        s.extraState = twice(() => initBuffer(gl, extra, 4))
        s.gravity = initBuffer(gl, gravity, 4)
        s.displayRadius = initBuffer(gl, displayRadius, 4)
        s.behavior = { keys: B.keys }
        for (let k of B.keys) s.behavior[k] = initBuffer(gl, B[k], 4)

        // If at the main menu, display UI.
        if (L.isMenu) {
            storeGet('menu').then(_ => {
                setTimeout(() => {
                    api.window([{style:'font-size:2em; letter-spacing:2px; text-align:center'}, 'LENIA GAME'], 'title', null, 0)
                    api.window(api.levelSelection(), 'levels', null, 0)
                }, 100) // This is for first-time visitors, waiting until `api.levelSuggest` is *probably* done.
            })
        }

        function b() { return new Float32Array(L._actorNames.length*4) }
        function twice(f) { return { prev:f(), next:f() } }
    }
    function updateActor(L, a) {
        if (!L._buffers) return
        const { pos, extra, gravity, displayRadius, B } = L._buffers
        const a2 = a.like != null && L.actors[a.like] || empty, i = a.i
        for (let c=0; c<4; ++c) pos[i*4+c] = a.pos && a.pos[c] || a2.pos && a2.pos[c] || 0
        for (let c=0; c<2; ++c) gravity[i*4+c] = a.gravity && a.gravity[c] || a2.gravity && a2.gravity[c] || 0
        for (let c=0; c<3; ++c) displayRadius[i*4+c] = a.displayRadius && a.displayRadius[c] || a2.displayRadius && a2.displayRadius[c] || 0
        const targetName = a.target && a.target || a2.target && a2.target || 0
        a._targetActor = L.actors[targetName] || null
        extra[i*4+0] = a.health = a._health == null ? a2._health || 1 : a.health
        extra[i*4+2] = a.emitRadius || a2.emitRadius || 10
        const color = a.emit || a2.emit
        extra[i*4+3] = color==='blue' ? 2 : color==='green' ? 1 : 0
        a._health = a._health || 0
        updateActorHealth(L, a)
        a._health = a.health
        for (let Bout = 0; Bout < Boutputs.length; ++Bout) {
            const kOut = Boutputs[Bout]
            const props = a[kOut] || a2[kOut], props2 = a2[kOut]
            if (props)
                for (let kIn of B.keys)
                    B[kIn][i * Boutputs.length + Bout] = props[kIn] || props2 && props2[kIn] || (kIn === 'B1' && typeof props == 'number' && props) || 0
        }
    }
    function updateActorWebGLGravity(L, start, end) {
        // Doesn't deal with end<start. But we don't use that anyway.
        const s = glState, b = L._buffers
        const i = start*4*4, n = start*4, m = end*4
        s.gravity.set(gl, i, b.gravity.subarray(n, m))
    }
    function updateActorWebGLData(L, start, end = start+1) {
        // Intended to be called after `updateActor`, with `actor.i` as `start`.
        const s = glState, b = L._buffers
        const i = start*4*4, n = start*4, m = end*4
        s.posSpeed.prev.set(gl, i, b.pos.subarray(n, m))
        s.posSpeed.next.set(gl, i, b.pos.subarray(n, m))
        s.extraState.prev.set(gl, i, b.extra.subarray(n, m))
        s.extraState.next.set(gl, i, b.extra.subarray(n, m))
        updateActorWebGLGravity(L, start, end)
        s.displayRadius.set(gl, i, b.displayRadius.subarray(n, m))
        for (let k of s.behavior.keys)
            s.behavior[k].set(gl, i, b.B[k].subarray(n, m))
    }
    function updateLevelWebGLData(L) {
        const s = glState
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
    }
    function updateActorCPUData(L, len, start) { // Returns `[…, health, score, _, _, …]`, with `len*4` numbers.
        // This sync GPU->CPU transfer may slow the game down.
        const s = glState
        const data = new Float32Array(len * 4)
        const maxLen = L._actorNames.length, firstLen = Math.min(len, maxLen - start)
        if (L._webglLost) return data
        gl.bindBuffer(gl.ARRAY_BUFFER, s.posSpeed.prev.buf)
        gl.getBufferSubData(gl.ARRAY_BUFFER, start*4*4, data, 0, firstLen*4)
        firstLen && gl.getBufferSubData(gl.ARRAY_BUFFER, (firstLen+start)%maxLen*4*4, data, firstLen*4)
        for (let I = start; I < start + len; ++I) {
            const i = I % maxLen
            const a = L.actors[L._actorNames[i]], p = a.pos, j = I-start
            p[0] = data[j*4 + 0], p[1] = data[j*4 + 1], p[2] = data[j*4 + 2], p[3] = data[j*4 + 3]
        }
        gl.bindBuffer(gl.ARRAY_BUFFER, s.extraState.prev.buf)
        gl.getBufferSubData(gl.ARRAY_BUFFER, start*4*4, data, 0, firstLen*4)
        firstLen && gl.getBufferSubData(gl.ARRAY_BUFFER, (firstLen+start)%maxLen*4*4, data, firstLen*4)
        return data
    }
    function updateActorHealth(L, a) {
        const wasOk = !!L._trackedLost
        const diff = a._health<=0 && a.health>0 ? 1 : a._health>0 && a.health<=0 ? -1 : 0
        if (a.trackLost && L._trackedLost != null) L._trackedLost += diff
        if (wasOk && !L._trackedLost) { // Lost the level, possibly after winning.
            if (L.score < L.winScore) { // Did not win.
                try {
                    if (typeof L.onLost == 'string') L.onLost = new Function('api,level', L.onLost)
                    if (typeof L.onLost == 'function') L.onLost(api, L)
                } catch (err) { console.error(err) }
            }
            if (!L.isMenu) api.levelSuggest(L.url, { lost:L.score })
        }
    }
    function handleExtraData(L, len = 256, start = Math.random() * L._actorNames.length | 0) { // Health & score.
        if (!glState.leniaFrames) return
        const maxLen = L._actorNames.length
        if (len > maxLen) len = maxLen, start = 0
        const data = updateActorCPUData(L, len, start)
        const gravity = L._buffers.gravity
        for (let I = 0; I < len; ++I) {
            const i = (start+I) % maxLen
            const name = L._actorNames[i], a = L.actors[name]
            if (L._trackedLost == null || L._trackedLost)
                L.score -= (a.score || 0) - (a.score = data[I*4+1])

            // Update target positions. (May be slow to propagate, but much better than WebGL-only "targets are impossible to implement unless we forego attributes and do everything through textures".)
            gravity[i*4+2] = a._targetActor ? a._targetActor.pos[0] : .5
            gravity[i*4+3] = a._targetActor ? a._targetActor.pos[1] : .5

            const health = data[I*4+0]
            if (a.health > 0 && health <= 0) {
                a.health = health
                try {
                    if (typeof a.onLost == 'string') a.onLost = new Function('api,level,actorName', a.onLost)
                    if (typeof a.onLost == 'function') a.onLost(api, L, name)
                } catch (err) { console.error(err) }
                updateActorHealth(L, a)
            }
            a._health = a.health = health
        }
        updateActorWebGLGravity(L, start, Math.min(start+len, maxLen))
        start+len > maxLen && updateActorWebGLGravity(L, 0, start+len - maxLen)
        // Update the displayed score.
        if (L.score >= L.winScore && !L._didWin) { // Won the level.
            try {
                if (typeof L.onWon == 'string') L.onWon = new Function('api,level', L.onWon)
                if (typeof L.onWon == 'function') L.onWon(api, L)
            } catch (err) { console.error(err) }
            if (!L.isMenu) api.levelSuggest(L.url, { won:L.frame, lost:L.score })
            L._didWin = true
        }
        const score = document.getElementById('score')
        score.textContent = !L.isMenu ? L.score.toFixed(2) + '/' + L.winScore.toFixed(0) : L.score ? L.score.toFixed(2) : ''
        if (!L.isMenu && L._lost) score.textContent += '\nbest ' + L._lost.toFixed(2)
        !L.isMenu && score.classList.toggle('win', !!L._didWin)
        score.classList.toggle('lost', L._trackedLost != null && !L._trackedLost)
        // Update the displayed time.
        const time = document.getElementById('time')
        time.textContent = !L.isMenu ? (L.frame / 60).toFixed(2) + 's' : ''
        if (!L.isMenu && L._won) time.textContent += '\nbest ' + (L._won / 60).toFixed(2) + 's'
        if (!L._didWin && L._trackedLost !== 0) ++L.frame
    }
    function draw() {
        requestAnimationFrame(draw)
        if (document.visibilityState === 'hidden') return
        maybeResize(canvas, canvas)
        gl.clear(gl.COLOR_BUFFER_BIT)
        if (!api._level) return
        initSound()
        const s = glState, L = api._level, p1 = s.lenia, p2 = s.actors, p3 = s.displayLenia, p4 = s.displayActors, rect = s.posBuffer
        handleLevelLoaded(s, L)
        if (p1 !== null) { // Lenia.
            const u = p1.uniform, a = p1.attrib
            gl.useProgram(p1.program)
            // Fill in the uniforms.
            gl.uniform1f(u.iTime, performance.now() / 1000)
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

            s.leniaFrames.next.copyTo(gl, s.leniaFrames.extra)
            s.leniaFrames.next.resetWrite(gl)
        }
        if (p2 !== null) { // Make actors act.
            const u = p2.uniform, a = p2.attrib
            gl.useProgram(p2.program)
            gl.uniform1f(u.iTime, performance.now() / 1000)
            gl.uniform4f(u.iMouse, mouse.x * L.width, (1 - mouse.y) * L.height, mouse.main, mouse.aux)
            gl.uniform4f(u.iResolution, L.width, L.height, 0, 0)
            s.leniaFrames.next.read(gl, 0, u.leniaGrid)
            s.leniaFrames.extra.write(gl)
            for (let i = 0; i < s.behavior.keys.length; ++i) {
                const k = s.behavior.keys[i]
                s.behavior[k].read(gl, a[k])
            }
            s.posSpeed.next.write(gl, 0)
            s.extraState.prev.read(gl, a.extraState)
            s.extraState.next.write(gl, 1)
            s.gravity.read(gl, a.gravity)
            gl.enable(gl.BLEND), gl.blendFunc(gl.ONE, gl.ONE)
            s.posSpeed.prev.draw(gl, a.posSpeed, gl.POINTS, true)
            gl.disable(gl.BLEND), gl.blendFunc(gl.ONE, gl.ZERO)
            s.extraState.next.resetWrite(gl, 1)
            s.posSpeed.next.resetWrite(gl, 0)
            s.leniaFrames.extra.resetWrite(gl)
            swap(s.posSpeed), swap(s.extraState)
        }
        if (p3 !== null) { // Display Lenia state.
            const u = p3.uniform, a = p3.attrib
            gl.useProgram(p3.program)
            gl.uniform4f(u.iResolution, L.width, L.height, 0, 0)
            gl.uniform4f(u.iDisplay, gl.drawingBufferWidth, gl.drawingBufferHeight, 0, 0)
            s.leniaFrames.extra.read(gl, 0, u.leniaGrid)
            s.leniaKernel.read(gl, 1, u.leniaKernel)
            gl.uniformMatrix4fv(u.iColorMatrix, false, L.iColorMatrix)
            rect.draw(gl, a.vertexPos)
        }
        if (p4 !== null) { // Display actors.
            const u = p4.uniform, a = p4.attrib
            gl.useProgram(p4.program)
            gl.uniform1f(u.iTime, performance.now() / 1000)
            gl.uniform4f(u.iDisplay, gl.drawingBufferWidth, gl.drawingBufferHeight, self.devicePixelRatio || 1, 0)
            s.leniaFrames.extra.read(gl, 0, u.leniaGrid)
            s.posSpeed.prev.read(gl, a.posSpeed)
            s.extraState.prev.read(gl, a.extraState)
            gl.uniformMatrix4fv(u.iColorMatrix, false, L.iColorMatrix)
            gl.enable(gl.BLEND), gl.blendFunc(gl.ONE, gl.SRC_COLOR)
            s.displayRadius.draw(gl, a.displayRadius, gl.POINTS)
            gl.disable(gl.BLEND), gl.blendFunc(gl.ONE, gl.ZERO)
        }

        swap(s.leniaFrames, 'prev', 'extra')
        gl.flush()

        handleExtraData(L)
    }
    function swap(a, k1='prev', k2='next') { [a[k1], a[k2]] = [a[k2], a[k1]] }



    function maybeResize(canvas, sizeToElem) {
        const dpr = self.devicePixelRatio || 1
        const w = sizeToElem.clientWidth * dpr | 0
        const h = sizeToElem.clientHeight * dpr | 0
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
        // An array of f32 values, `r: Float32Array`.
        //   Read with `r.read(gl, gl.getAttribLocation(program, 'attrib'))` as a vertex attribute.
        //   Draw this list of vertices with `r.draw(gl, gl.getAttribLocation(program, 'attrib'), gl.POINTS, false)`.
        //   Write with `r.write(gl, 0)` then `r.resetWrite(gl)` as transform-feedback of a vertex shader.
        //     If there are any buffer writes, pass `true` to `.draw`.
        const buf = gl.createBuffer()
        gl.bindBuffer(gl.ARRAY_BUFFER, buf)
        gl.bufferData(gl.ARRAY_BUFFER, f32, usageHint)
        return {
            buf,
            length: f32.length / numbersPerValue | 0,
            numbersPerValue,
            read(gl, attribLocation) {
                if (attribLocation < 0) return
                gl.bindBuffer(gl.ARRAY_BUFFER, this.buf)
                gl.enableVertexAttribArray(attribLocation)
                gl.vertexAttribPointer(attribLocation, this.numbersPerValue, gl.FLOAT, false, 0, 0)
            },
            draw(gl, attribLocation, mode = gl.TRIANGLE_STRIP, needsTransformFeedback = false) {
                this.read(gl, attribLocation)
                if (needsTransformFeedback) gl.beginTransformFeedback(gl.POINTS)
                gl.drawArrays(mode, 0, this.length)
                if (needsTransformFeedback) gl.endTransformFeedback()
            },
            write(gl, index = 0) {
                gl.bindBufferBase(gl.TRANSFORM_FEEDBACK_BUFFER, index, this.buf)
            },
            resetWrite(gl, index) {
                gl.bindBufferBase(gl.TRANSFORM_FEEDBACK_BUFFER, index, null)
            },
            set(gl, offset, data) {
                // Overwrites GPU-side data.
                gl.bindBuffer(gl.ARRAY_BUFFER, this.buf)
                gl.bufferSubData(gl.ARRAY_BUFFER, offset, data)
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
            width, height,
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
                gl.viewport(0,0, this.width, this.height)
            },
            copyTo(gl, texture) {
                gl.bindTexture(gl.TEXTURE_2D, texture.R)
                gl.copyTexImage2D(gl.TEXTURE_2D, 0, gl.RGBA, 0, 0, this.width, this.height, 0)
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
        return fetch(url, {
            mode:'cors',
            cache: localStorage.debug == 'true' ? 'reload' : 'force-cache',
        }).then(response => response.json())
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

    function storeSet(key, value) { return localStorage[key] = value, Promise.resolve() }
    function storeGet(key) { return Promise.resolve(localStorage[key]) }

    function urlsToHierarchy(urls) {
        // Converts `{ url:value }` to an object hierarchy such as `{ 'levels/directory1/directory2':{ 1:550, 2:-0.2 } }`, suitable for displaying.
        // Ex: `urlsToHierarchy({'a/b/c.json':5, 'a/b/d.json':6})` → `{'a/b':{ c:5, d:6 }}`
        const result = Object.create(null)
        for (let k in urls) {
            const v = urls[k], parts = new URL(k, location).pathname.slice(1).split('/')
            if (parts[0] !== 'levels') parts.unshift('levels')
            parts.length && (parts[parts.length-1] = parts[parts.length-1].replace('.json', ''))
            for (let i = 0, o = result; i < parts.length; ++i)
                parts[i] = (parts[i][0].toUpperCase() + parts[i].slice(1)).replace(/\-\_/g, ' '),
                o = o[parts[i]] = i < parts.length-1 ? (o[parts[i]] || Object.create(null)) : v
        }
        return mergeSingles(result)
        function mergeSingles(x, up = false) { // {levels:{directory1:{directory2:{…}}}} → {'levels/directory1/directory2':{…}}
            if (!x || typeof x != 'object' || Object.getPrototypeOf(x) !== null) return x
            const keys = Object.keys(x), k = keys[0]
            if (keys.length != 1) return x
            if (!up)
                for (let k of keys)
                    x[k] = mergeSingles(x[k])
            if (!x[k] || typeof x[k] != 'object' || Object.getPrototypeOf(x[k]) !== null) return x
            const keys2 = Object.keys(x[k]), k2 = keys2[0]
            if (keys2.length != 1) return x
            const r = Object.create(null)
            r[k + ' / ' + k2] = x[k][k2]
            return mergeSingles(r, true)
        }
    }

    function initSound() {
        if (typeof sn == 'undefined' || api._soundHandler) return
        api._soundHandler = new sn.Handler.Sound({volume:.3,minFrequency:0,maxFrequency:999999999})
    }
})(document.getElementById('main'), self)