#coding=utf-8
from PIL import Image#需要pillow库
import glob, os
in_dir ='background'#源图片目录
out_dir = in_dir+'_out'#转换后图片目录
if not os.path.exists(out_dir): os.mkdir(out_dir)

#图片批处理
def main():
    for files in glob.glob(in_dir+'/*'):
        filepath,filename = os.path.split(files)
        im = Image.open(files)
        w,h = im.size
        im = im.resize((int(1920), int(1.0*h/w*1920)))
        im.save(os.path.join(out_dir,filename))

if __name__=='__main__':
    main()