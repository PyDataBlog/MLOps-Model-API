import os, random
rfilename=random.choice(os.listdir("/storage/pictures"))
rextension=os.path.splitext(rfilename)[1]
picturespath='/storage/pictures/'

#TODO Probably dont need a forloop can possibly do random*
#TODO What if the directory is empty?
for filename in os.listdir(picturespath):
  if filename.startswith("random"):
	extension=os.path.splitext(filename)[1]
	newname=picturespath + str(random.random()).rsplit('.',1)[1] + extension
	# rename the existing random wallpaper to something random
	filename=picturespath+filename
	os.rename(filename, newname)



# now rename the newly randomly founded file to be random
rfilename=picturespath+rfilename
os.rename(rfilename, picturespath+'random'+rextension)
