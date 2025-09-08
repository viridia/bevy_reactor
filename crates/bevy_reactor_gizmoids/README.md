# bevy_reactor_gizmoids

## What's a Gizmoid?

The Latin suffix "-oid" means "having the form of" (e.g. _spheroid_, _android_, and so on). In
this case, a "Gizmoid" is an object having the form of a Gizmo, but somewhat different.

Gizmoids are dynamic meshes, a Bevy `Mesh` object whose vertices are computed by a formula, and
which is updated reactively when the inputs to that formula change. Unlike regular Gizmos, it's
a retained rather than immediate-mode construct.

Unlike regular gizmos, gizmoids are not limited to just line-drawing primitives, but can general
triangle meshes as well.

The `gizmoid()` template function accepts as a parameter a draw function, which is provided with
a drawing API that can draw stroked or filled shapes, which are then converted to triangles.
Although the API looks much like a traditional 2D drawing interface, the resulting mesh is not
limited to existing in a flat plane. This can be useful for things like drawing lines on a terrain
map.
