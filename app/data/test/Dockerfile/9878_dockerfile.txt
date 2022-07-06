FROM jupyter/scipy-notebook

USER root
RUN apt install -y quantlib-python 

USER $NB_UID

RUN echo "updating conda" \
 && conda update --all --yes \
 && echo "pip install" \
 && pip install --upgrade \
    'pymc3>=3.11' \
    "torch>=1.7" torchvision torchaudio pytorch-lightning \
    yfinance ib_insync watermark tensorflow "numpy>=1.20" \
    jupyterlab_hdf hdf5plugin xgboost \
 && echo "jupyter install" \
 && jupyter labextension install @jupyterlab/katex-extension @jupyterlab/celltags 
 # @jupyterlab/hdf5
 
 
#   @jupyterlab/git
#RUN conda install -c conda-forge "elyra>=2.0.1" && jupyter lab build
 
RUN pip list \
 && conda info \
 && conda list \
 && jupyter labextension list
