#ref:https://www.tensorflow.org/tutorials/keras/classification?hl=zh-cn
#http://www.keyan.me/09-%E6%B7%B1%E5%BA%A6%E5%AD%A6%E4%B9%A0%E4%B8%8ECV-01/chapter4/section9.html
#https://github.com/cchangcs/trash-classification/blob/master/src/predict.py 


import os
from PIL import Image
import numpy as np
import tensorflow.compat.v1 as tf
tf.disable_v2_behavior() 
from tensorflow import keras
import pandas as pd


data_dir = "C:\\Users\\60184\\Desktop\\STAT440\\M3\\garbage\\tr"
train = True
model_path = "C:\\Users\\60184\\Desktop\\STAT440\\M3"

def read_data(data_dir):
    datas = []
    labels = []
    fpaths = []
    for fname in os.listdir(data_dir):
        if fname.endswith(".jpg"):
            fpath = os.path.join(data_dir, fname)
            fpaths.append(fpath)
            image = Image.open(fpath)
            data = np.array(image,dtype='uint8') / 255.0
            datas.append(data)
        elif fname.endswith(".txt"):
            fpath = os.path.join(data_dir, fname)
            info = open(fpath,'r')
            a = info.read().splitlines()
            a = pd.to_numeric(a)-1
            labels = labels + list(a)
    datas = np.array(datas,dtype='float32')
    
    return fpaths, datas, labels


fpaths, datas, labels = read_data(data_dir)

#count the number of classes
num_classes = len(set(labels))


#read the training data


data_dir_test = "C:\\Users\\60184\\Desktop\\STAT440\\M3\\garbage\\te"
def read_data(data_dir_test):
    datas_test = []
    fpaths_test = []
    labels_test = []
    for fname in os.listdir(data_dir_test):
        if fname.endswith(".jpg"):
            fpath = os.path.join(data_dir_test, fname)
            fpaths_test.append(fpath)
            image = Image.open(fpath)
            image = tf.image.per_image_standardization(image)
            data = np.array(image,dtype='uint8') / 255.0
            datas_test.append(data)
            labels_test=labels_test+list('')
            
    datas_test = np.array(datas_test,dtype='float32')
    return fpaths_test, datas_test,labels_test

fpaths_test, datas_test,labels_test= read_data(data_dir_test)


#All three models not performing better than baseline
#Image rescale is not been done, only did image/255 by size

#model = keras.Sequential([
 #   keras.Input(shape=(384,512,3)),
 #   keras.layers.Conv2D(32, (3, 3), padding='same'),
# #   keras.layers.MaxPooling2D(),
 #   keras.layers.Conv2D(32, 3, padding='same', activation='relu'),
 #   keras.layers.MaxPooling2D(),
 #   keras.layers.Conv2D(64, 3, padding='same', activation='relu'),
 #   keras.layers.MaxPooling2D(),
 #   keras.layers.Flatten(),
 #   keras.layers.Dense(128, activation='relu'),
 #   keras.layers.Dense(6)
 #   ])

model = keras.Sequential([
    keras.layers.Conv2D(filters=32, kernel_size=(3, 3), padding="same"),
    keras.layers.Activation("relu"),
    keras.layers.Conv2D(filters=32, kernel_size=(3, 3), padding="same"),
    keras.layers.Activation("relu"),
    keras.layers.MaxPooling2D(pool_size=(2, 2)),
    keras.layers.BatchNormalization(),
    keras.layers.Dropout(0.25),
    keras.layers.Conv2D(filters=64, kernel_size=(3, 3), padding="same"),
    keras.layers.Activation("relu"),
    keras.layers.Conv2D(filters=64, kernel_size=(3, 3), padding="same"),
    keras.layers.Activation("relu"),
    keras.layers.MaxPooling2D(pool_size=(2, 2)),
    keras.layers.BatchNormalization(),
    keras.layers.Dropout(0.25),
    keras.layers.Conv2D(filters=128, kernel_size=(3, 3), padding="same"),
    keras.layers.Activation("relu"),
    keras.layers.Conv2D(filters=128, kernel_size=(3, 3), padding="same"),
    keras.layers.Activation("relu"),
    keras.layers.MaxPooling2D(pool_size=(2, 2)),
    keras.layers.BatchNormalization(),
    keras.layers.Dropout(0.25),
    # flattening the convolutions
    keras.layers.Flatten(),
    keras.layers.Dense(6)
    ])


model = keras.Sequential([
    keras.layers.Conv2D(filters=32, kernel_size=(3, 3), padding="same"),
    keras.layers.Activation("relu"),
    keras.layers.MaxPooling2D(pool_size=(2, 2)),
    keras.layers.BatchNormalization(),
    keras.layers.Dropout(0.25),
    keras.layers.Conv2D(filters=64, kernel_size=(3, 3), padding="same"),
    keras.layers.Activation("relu"),
    keras.layers.MaxPooling2D(pool_size=(2, 2)),
    keras.layers.BatchNormalization(),
    keras.layers.Dropout(0.25),
    keras.layers.Conv2D(filters=128, kernel_size=(3, 3), padding="same"),
    keras.layers.Activation("relu"),
    keras.layers.MaxPooling2D(pool_size=(2, 2)),
    keras.layers.BatchNormalization(),
    keras.layers.Dropout(0.25),
    # flattening the convolutions
    keras.layers.Flatten(),
    keras.layers.Dense(6)
    ])



a_list = list(range(1264,2528))
labels=np.array(labels,dtype='float32')


model.compile(optimizer='adam',
              loss=tf.keras.losses.SparseCategoricalCrossentropy(from_logits=True),
              metrics=['accuracy'])
model.fit(datas, labels, epochs=5)

model.summary()

probability_model = tf.keras.Sequential([model, tf.keras.layers.Softmax()])
predictions = probability_model.predict(datas_test)

outs = []
for i in range(0,1264):
    out=np.argmax(predictions[i-1])+1
    outs.append(out)

finalout = pd.DataFrame()
finalout['Id'] = a_list
finalout['Prediction'] = outs
finalout.to_csv(r'C:\Users\60184\Desktop\STAT440\M3\finalout.csv')


tl, ta = model.evaluate(datas, labels)
print('Test accuracy: %0.3f (%% correct)' % (ta * 100))
