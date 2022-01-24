Each level specifies, in JSON, all metadata and all simulation parameters and all actors.

Making a new level is best done by copying a pre-existing level and changing its simulation parameters.

Levels are cached by default; to be able to reload levels that you are working on, do `localStorage.debug=true` in JS console (F12).

## Metadata

`description: string`: Markdown, displayed in level-select.

---

`isMenu=false: bool`: if `true`, level-select buttons will be displayed, and the main-menu will return to this level.

---

`score=0: number`: the current score, which is the sum of all actor scores.

---

`winScore=1: number`: how much score (a global number) must be accumulated for the level to be considered completed.

---

`onLoad`: when this level is loaded, this JS is called, with `api` and `level`.

---

`onWon`: when `level.score > level.winScore`, this JS is called, with `api` and `level`.

---

`onLost`: when all player's actors (with `trackLost`) have been lost, the level is lost, and this is called, with `api` and `level`.

---

## Simulation

[Lenia](https://arxiv.org/abs/1812.05433) is the main "physics" engine on this game. Each level can tune Lenia parameters.

`iColorMatrix: mat4`: controls presentation. Lenia-color is multiplied by this matrix to get displayed-color. Alpha is always 1, so it can be used to provide default illumination, and inverted colors.

---

`width: int` and `height: int`: Lenia physics are computed on a grid of this size, with kernel radius 5 for efficiency. Width/height must be powers of 2.

---

`iSlowdown: int`: slows down the computed changes by this much.

---

`iMixing: mat3`: defines how colors interact. The identity matrix makes colors independent, else they can create (>0) and destroy (<0) each other.

---

`kernel: object`: defines per-color 11×11 convolution kernels.

There are two options:

- `{ center:[r,g,b], width:[r,g,b] }`: creates Lenia kernels, which define how the distance to each neighboring pixel is transformed into its weight, via a bell-shaped curve (nothing if too little or too much, has to be just right).

- `{ r:mat11, g:mat11, b:mat11 }`: the raw values of neighbor contributions. Use this if you want non-wave behavior, blocky shapes, or funky textures.

For example:

```json
"kernel": {
    "g": [
        0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,1,0,0,0,0,0,
        0,0,0,0,1,1,0,0,0,0,1,
        0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,1
      ]
}
```

---

`iGrowthCenter: vec3`, `iGrowthWidth: vec3`: how the total neighbor contribution is transformed, via a bell-shaped curve. Specified per-color.

---

`iOffset: vec2`: wind; use `[0,0]` for stability. What if you want an open-world map, after all?

`iKernelOffset: vec2`: this originates from a buggy wind implementation, and alters the calculation of the distance to a neighbor. Many things look more visually interesting with this.

---

## Actors

Actors. Agents. Entities. Particles. Thingies.

`.actors`: an object containing the named actors. (Could be an array too, if names are unimportant.)

`.actors` contains:

---

`displayRadius=[0,0,0]: [r,g,b]`: creates an extra visual highlight.

---

`health=1: 0…1`: can be displayed with `displayRadius`, and reacted-to. Represents structural integrity. Once it hits `0`, it can never rise again, unless JS wills it to.

---

`trackLost=false`: whether the level should call `onLost` when all that have this have depleted their structural integrity.

---

`onLost: string`: specifies the body of a JS function, called when `health` first reaches `0`, with `api` and `level` and `actorName` as args.

---

`onLoad: string`: what to do when the agent is first loaded.

---

`emitRadius=10`: actors can emit Lenia colors, and this determines the radius of that emittance.

---

`emit: "red"|"green"|"blue"`: what color to emit.

---

`gravity: [x,y]`: this is added to velocity/speed each frame. Can't prefer one direction (down: `[0, -0.0005]`) without this.

---

`target: ActorName`: actors can react to their target actor's position, and be attracted or repulsed by it.

---

`like: ActorName`: specify this to inherit (copy) properties from another actor. You don't have to copy-paste with this.

Only one layer of copying here, though.

---

#### Actor behavior

Each actor is very unique and special, and can do things that no other actor can.

Actor behavior is determined by the behavior matrix, which inputs are multiplied by to compute outputs.

---

Outputs, in the actor object:
- `speed` (computed separately for x and y, but with the same weights in the behavior matrix)
- `emittance`
- `dhealth`
- `dscore`

---

Inputs, in an output's object (or specify a number to make the output constant), each weighed/multiplied by the specified number:

- `B1`: the `1` input. Constant. Uniform. Bias. B for Behavior.
- `Bspeed`: the current speed. For example, when computing `speed`, set `Bspeed=1` to make the rest compute acceleration, or `Bspeed=0.99` to slow down gradually.
- `Bmouse`: how far away the mouse is, as a vec2 with -1…1 components.
- `Btarget`: how far away the target is, as a vec2 with -1…1 components.
- `Bhealth`: the current health.
- Colors:
  - `Br`: the intensity/direction of Lenia RED nearby, 0…1 or -1…1. Examples: for `speed`, `Bdr=-0.01` implements collision detection; for `emittance`, this can be used for traps that wait for a color to emit their own.
  - `Bg`: the intensity/direction of Lenia GREEN nearby.
  - `Bb`: the intensity/direction of Lenia BLUE nearby.
- `Btime`: `sin(BtimeFrequency * time * 2*PI)`, -1…1, changing periodically.
- `BtimeFrequency`: how often the value of `Btime` repeats itself, in 1/seconds.

---

## JS API

Some methods here begin with `on`, and define a string. These are JS callbacks. They are usually given `api, level, actorName` as arguments.

For example, specifying `onLost: "setTimeout(() => api.levelLoad(level.url), 1000)"` or `onLost: "setTimeout(api.levelLoad, 1000)"` would reload the level on losing, after 1 second.

`api` contains:

---

`api.levelLoad(url = level.url)`: goes to a level. `url` must point to a JSON file of the level.

---

`api.share()`: returns a URL that opens the current level when navigated to.

---

`api.levelSuggest(url, winLose = {lost:0})`: remembers information about a level.
- Given `url`, remembers it, to recommend to the user later. 🌟 Preferred 🌟
- Given `url` and `{ won:level.frame, lost:level.score }`, may update the min stored time.
- Given `url` and `{ lost:level.score }`, such as on level lose or end, may update the max stored score.
- Given nothing, fetches a promise of `{ won:{url:time}, lost:{url:score} }` for all URLs. Won levels are in both, non-won ones are in `lost`.
- Given `url` and `{}`, forgets the level's data.

---

`api.levelExit()`: goes to the last-visited main menu level.

---

`api.read(actorName)`: syncs in-GPU speed to our in-CPU actor object: position, speed, health, score. Usually useless, because they are intermittently synchronized anyway for other needs.

---

`api.write(actorName)`: after changing an actor object's props, call this to sync changes to in-GPU state.

(Yes, actors are updated in-GPU, so the engine can handle hundreds of thousands of actors.)

`api.write()`: for updating the kernel. (But not the actor list. Actors never disappear and never appear.)

---

`api.window(content, actorName = null, timeoutSec = 32, posMomentum = .9)`: creates a window, near an actor. For STORY.
- Given a string or a DOM element or an array tree, and the actor name, positions a window that follows the actor.
- Given a string or a DOM element or an array tree, positions a free-floating window in the bottom-left corner.
- To not fade away after `timeoutSec`, pass `timeoutSec = null`.
- Given nothing, clears every window instantly. (Level load does this.)
- Returns a promise, which resolves when the timeout has passed.

When the user clicks, all active windows take 4 less seconds to disappear. Click repeatedly to skip the story.

If `posMomentum` is `0`, the window is centered on the element, instead of daintily following it. Otherwise, the closer it is to `1`, the slower the window follows.

`content` can be of the format shown in the example `[{ tag:'div', style:'color:red', onclick() { api.levelLoad() } }, 'Click to ', [{ style:'color:blue' }, 'reload'], ' the level']`, for convenience.

---

`api.levelSelection()`: a menu for selecting a level to go to. Perfect `content` for a `window`.

`api.levelSelection(true)`: returns how many novel (non-won, but in storage) levels we are currently aware of.

---