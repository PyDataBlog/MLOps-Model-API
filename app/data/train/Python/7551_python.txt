from OpenGLCffi.GL import params
@params(api='gl', prms=['pname', 'index', 'val'])
def glGetMultisamplefvNV(pname, index, val):
	pass


@params(api='gl', prms=['index', 'mask'])
def glSampleMaskIndexedNV(index, mask):
	pass


@params(api='gl', prms=['target', 'renderbuffer'])
def glTexRenderbufferNV(target, renderbuffer):
	pass


