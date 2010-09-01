#!BPY
"""
Name: 'MyMeshExport'
Blender: 248
Group: 'Export'
Tooltop: 'Export a MyMesh File'
"""

try:
    import Blender
    import bpy
except:
    print "not running in blender"

def render_sexp(lol):
    if type(lol) is list:
        return '(' + ' '.join([render_sexp(x) for x in lol]) + ')'
    elif isinstance(lol, basestring):
        return ":%s" % lol
    else:
        return str(lol)

def v2l(v):
    "convert vertex into list"
    return [v.co.x, v.co.y, v.co.z, v.no.x, v.no.y, v.no.z]

def f2l(f):
    "convert face into list"
    return [v.index for v in f.v]

def write(filename):
    out = file(filename, "w")
    sce = bpy.data.scenes.active

    # selected objects if there are any. active otherwise
    objs = sce.objects.selected
    if not objs:
        objs = [sce.objects.active]

    result = []
    for obj in objs:
        mesh_copy = Blender.Mesh.New()
        mesh_copy.getFromObject(obj.name)

        mesh_copy.transform(obj.getMatrix(), recalc_normals=1)

        verts = ["vert", [v2l(v) for v in mesh_copy.verts]]
        faces = ["face", [f2l(f) for f in mesh_copy.faces]]

        result.append([obj.getName(), [verts, faces]])

    out.write(render_sexp(result))
    out.close()

try:
    Blender.Window.FileSelector(write, "Export")
except:
    pass

if __name__=="__main__":
    print render_sexp(["mesh", [[0, 1, 2], [2, 1, 3], [3, 0, 1]]])
