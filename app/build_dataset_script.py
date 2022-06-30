import os
import multiprocessing as mp
from time import time
from datasets import load_dataset
from functools import partial
from app.utils import supported_langs_dict


def build_train_val_test(n: int, lang: str) -> bool:
    # Build train, val and test datasets for a given language using a ratio of 0.8, 0.1 and 0.1
    # The datasets are saved in the data folder
    train_thr = int(n * 0.8)
    val_thr = int(n * 0.9)
    dataset_stream = load_dataset("lvwerra/github-code", streaming=True, split="train", languages=[lang])
    counter = 0

    # Iterate through the dataset and save the data in the train, val and test folders as a text file
    for i in iter(dataset_stream):
        if counter < train_thr:
            os.makedirs(f"app/data/train/{lang}", exist_ok=True)
            with open(f"app/data/train/{lang}/{counter}_{lang.lower()}.txt", "w") as f:
                f.write(i["code"])
        elif counter < val_thr:
            os.makedirs(f"app/data/val/{lang}", exist_ok=True)
            with open(f"app/data/val/{lang}/{counter}_{lang.lower()}.txt", "w") as f:
                f.write(i["code"])
        else:
            os.makedirs(f"app/data/test/{lang}", exist_ok=True)
            with open(f"app/data/test/{lang}/{counter}_{lang.lower()}.txt", "w") as f:
                f.write(i["code"])
        counter += 1
        if counter == n:
            break
    return True


if __name__ == "__main__":
    start = time()
    pool = mp.cpu_count()
    n = len(supported_langs_dict.keys())
    with mp.Pool(min(n, pool)) as p:
        res = p.map(partial(build_train_val_test, 10), supported_langs_dict.keys())
        print(f"Parallel build took {time() - start} seconds")
        print(res)
