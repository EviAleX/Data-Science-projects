# 🧠 CNN-Based Edge Detection on Grayscale Image

This project demonstrates the practical application of **Convolutional Neural Networks (CNNs)** for image processing, specifically for **edge detection** on a grayscale photograph of SGH (Warsaw School of Economics).

## 🧾 Overview

The goal of this project is to explores how a simple convolutional layer can highlight key image features. Using **Keras** and **TensorFlow**, we apply both randomly initialized and custom-designed convolution filters to a grayscale image.

## 🎯 Goal

The main objective is to show how **convolution operations**, central to CNNs, can automatically extract meaningful patterns such as **vertical and horizontal edges**. This process illustrates one of the foundational principles of modern image processing.

## 🧪 What Was Done

- Loaded and converted a grayscale image for CNN input
- Built a minimal CNN model with a single `Conv2D` layer
- Applied both random and custom-defined convolution filters
- Visualized how convolution emphasizes structural elements (edges) of the image

## 📷 Result

The model successfully detected dominant **edges** in the input image by using a custom-designed 5×5 convolution kernel. This visually highlights the power of convolutional operations in feature extraction.
