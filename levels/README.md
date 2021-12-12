Each level specifies, in JSON, all metadata and all simulation parameters and all entities.

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
{
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