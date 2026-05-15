#!/usr/bin/env python3
"""
Phishing Website Detector - Model Performance Analysis
Analyzes the Random Forest model trained on phishing detection dataset
"""

import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import (accuracy_score, precision_score, recall_score, 
                             f1_score, classification_report, confusion_matrix)
import json

# Load dataset
df = pd.read_csv('cyber crime phising analysis/dataset.csv')

# Separate features and target
X = df.drop('Result', axis=1)
y = df['Result']

# Map target labels to binary (1 for phishing, -1 for legitimate)
y_binary = (y == 1).astype(int)

# Split data (70-30 split as per existing code)
X_train, X_test, y_train, y_test = train_test_split(
    X, y_binary, test_size=0.3, random_state=123
)

# Train Random Forest model
print("=" * 70)
print("PHISHING WEBSITE DETECTOR - MODEL PERFORMANCE ANALYSIS")
print("=" * 70)
print(f"\nDataset Size: {len(df)} samples")
print(f"Training Set: {len(X_train)} samples")
print(f"Test Set: {len(X_test)} samples")
print(f"Features: {X.shape[1]}")

model = RandomForestClassifier(n_trees=200, random_state=123, n_jobs=-1)
model.fit(X_train, y_train)

# Make predictions
y_pred = model.predict(X_test)
y_pred_proba = model.predict_proba(X_test)[:, 1]

# Calculate metrics
accuracy = accuracy_score(y_test, y_pred)
precision = precision_score(y_test, y_pred)
recall = recall_score(y_test, y_pred)
f1 = f1_score(y_test, y_pred)

print("\n" + "=" * 70)
print("MODEL PERFORMANCE METRICS")
print("=" * 70)
print(f"\nAccuracy:  {accuracy:.4f} ({accuracy*100:.2f}%)")
print(f"Precision: {precision:.4f} ({precision*100:.2f}%)")
print(f"Recall:    {recall:.4f} ({recall*100:.2f}%)")
print(f"F1 Score:  {f1:.4f}")

# Confusion Matrix
cm = confusion_matrix(y_test, y_pred)
print("\n" + "-" * 70)
print("CONFUSION MATRIX:")
print("-" * 70)
print(f"True Negatives:  {cm[0][0]}")
print(f"False Positives: {cm[0][1]}")
print(f"False Negatives: {cm[1][0]}")
print(f"True Positives:  {cm[1][1]}")

# Classification Report
print("\n" + "-" * 70)
print("DETAILED CLASSIFICATION REPORT:")
print("-" * 70)
print(classification_report(y_test, y_pred, target_names=['Legitimate', 'Phishing']))

# Save metrics to JSON
metrics_dict = {
    "model_name": "Random Forest Phishing Detector",
    "dataset_size": len(df),
    "train_size": len(X_train),
    "test_size": len(X_test),
    "num_features": X.shape[1],
    "num_trees": 200,
    "metrics": {
        "accuracy": float(accuracy),
        "accuracy_percentage": round(accuracy * 100, 2),
        "precision": float(precision),
        "precision_percentage": round(precision * 100, 2),
        "recall": float(recall),
        "recall_percentage": round(recall * 100, 2),
        "f1_score": float(f1)
    },
    "confusion_matrix": {
        "true_negatives": int(cm[0][0]),
        "false_positives": int(cm[0][1]),
        "false_negatives": int(cm[1][0]),
        "true_positives": int(cm[1][1])
    }
}

with open('model_metrics.json', 'w') as f:
    json.dump(metrics_dict, f, indent=2)

print("\n" + "=" * 70)
print(f"✓ Metrics saved to model_metrics.json")
print("=" * 70)

# Summary for STAR method
print("\n" + "=" * 70)
print("STAR METHOD PROJECT BULLET POINT:")
print("=" * 70)
print(f"\n📊 Achieved {precision*100:.2f}% precision rate in phishing detection model,")
print(f"   successfully identifying malicious websites with high accuracy")
print(f"   while minimizing false positives across {len(X_test)} test samples.")
print("=" * 70)
