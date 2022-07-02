import os
import logging
import tensorflow as tf
from typing import Dict, Tuple
from tensorflow.keras import layers
from tensorflow.keras import losses
from tensorflow.keras.layers import TextVectorization


logger = logging.getLogger(__name__)


os.environ["TF_FORCE_GPU_ALLOW_GROWTH"] = "true"
batch_size = 32
seed = 2022

train_ds = tf.keras.utils.text_dataset_from_directory("app/data/train", batch_size=batch_size, seed=seed)
val_ds = tf.keras.utils.text_dataset_from_directory("app/data/val", batch_size=batch_size, seed=seed)
test_ds = tf.keras.utils.text_dataset_from_directory("app/data/test", batch_size=batch_size, seed=seed)

class_mappings = {num: lbl for num, lbl in enumerate(train_ds.class_names)}

vectorizer = TextVectorization(max_tokens=20_000, output_mode="binary", ngrams=1)

train_text = train_ds.map(lambda text, label: text)
with tf.device("CPU"):
    vectorizer.adapt(train_text)


def vectorize_text(text: str, label: int) -> Tuple[tf.Tensor, tf.Tensor]:
    text = tf.expand_dims(text, -1)
    return vectorizer(text), label


def configure_dataset(dataset: tf.data.Dataset) -> tf.data.Dataset:
    return dataset.cache().prefetch(buffer_size=tf.data.AUTOTUNE)


def transform_datasets(
    train_set: tf.data.Dataset, val_set: tf.data.Dataset, test_set: tf.data.Dataset
) -> Tuple[tf.data.Dataset, tf.data.Dataset, tf.data.Dataset]:
    vec_train_ds = train_set.map(vectorize_text)
    vec_val_ds = val_set.map(vectorize_text)
    vec_test_ds = test_set.map(vectorize_text)

    vec_train_ds = configure_dataset(vec_train_ds)
    vec_val_ds = configure_dataset(vec_val_ds)
    vec_test_ds = configure_dataset(vec_test_ds)

    return (vec_train_ds, vec_val_ds, vec_test_ds)


def generate_model(train_set: tf.data.Dataset, val_set: tf.data.Dataset, test_set: tf.data.Dataset) -> bool:
    model = tf.keras.Sequential([layers.Dense(len(train_ds.class_names), activation="softmax")])

    model.compile(loss=losses.SparseCategoricalCrossentropy(), optimizer="adam", metrics=["accuracy"])

    history = model.fit(train_set, validation_data=val_set, epochs=4)

    test_loss, test_acc = model.evaluate(test_set)
    logger.info(f"Vectorization test loss: {test_loss}, Vectorizatio test accuracy: {test_acc}")

    exported_model = tf.keras.Sequential(
        [
            vectorizer,
            model,
        ]
    )

    exported_model.compile(
        loss=losses.SparseCategoricalCrossentropy(),
        optimizer="adam",
        metrics=["accuracy"],
    )
    # Evaluate the final model pipeline on the raw original test data
    final_test_loss, final_test_acc = exported_model.evaluate(test_ds)
    logger.info(f"Final test accuracy: {final_test_acc}, final test loss: {final_test_loss}")

    # Save the model
    model_name = "code_classifier"
    exported_model.save(f"app/models/{model_name}", save_format="tf")
    logger.info(f"{model_name} model saved successfully")
    return True


def train_model() -> Dict[str, str]:
    logger.info(f"Tensorflow version: {tf.__version__}")
    final_train, final_val, final_test = transform_datasets(train_set=train_ds, val_set=val_ds, test_set=test_ds)
    generate_model(train_set=final_train, val_set=final_val, test_set=final_test)
    logger.info("Model training complete")
    return {"Message": "Model trained successfully"}


if __name__ == "__main__":
    train_model()
