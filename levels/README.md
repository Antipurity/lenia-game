Each level specifies, in JSON, all metadata and all simulation parameters and all actors.

Making a new level is best done by copying a pre-existing level and changing its simulation parameters.

## Metadata

`description: string`: Markdown, displayed in level-select.

---

`isMenu: bool`: if `true`, level-select buttons will be displayed, and the main-menu will return to this level.

---

`winScore: number`: how much score (a global number) must be accumulated for the level to be considered completed.

---

`winUnlocks: number`: a list of level URLs that are unlocked.

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

`health=1`: has no inherent meaning, but actors can change their health and react to it.

---

`radius=10`: actors can emit Lenia colors, and this determines the radius of that emittance.

---

`emit: "red"|"green"|"blue"`: what color to emit.

---

`gravity: [x,y]`: this is added to velocity/speed each frame. Can't prefer one direction (down: `[0, -0.0005]`) without this.

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
- `Bmouse`: how far away the mouse is, as a vec2 with 0…1 components.
- `Btarget`: how far away the target is, as a vec2 with 0…1 components.
- `Bhealth`: the current health.
- Colors:
  - `Br`: the intensity/direction of Lenia RED nearby, 0…1. Examples: for `speed`, `Bdr=-0.01` implements collision detection; for `emittance`, this can be used for traps that wait for a color to emit their own.
  - `Bg`: the intensity/direction of Lenia GREEN nearby.
  - `Bb`: the intensity/direction of Lenia BLUE nearby.
- `Btime`: `sin(BtimeFrequency * time * 2*PI)`, -1…1, changing periodically.
- `BtimeFrequency`: how often the value of `Btime` repeats itself, in 1/seconds.

---