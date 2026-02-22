import random
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import binom
from tqdm import tqdm


def has_series(results):
    count = 0
    for r in results:
        if r == 1:
            count += 1
            if count >= 5:
                return True
        else:
            count = 0
    return False

def max_series_length(results):
    max_len = 0
    current_len = 0
    for r in results:
        if r == 1:
            current_len += 1
            max_len = max(max_len, current_len)
        else:
            current_len = 0
    return max_len

def default():
    count_heads = []
    n = 100
    p = 0.5

    alpha = 0.05
    count_row_of_5 = 0

    for _ in range(10 ** 6):
        int_mask = random.randint(0, 2 ** 100)
        results = []
        for i in range(100):
            results.append(int_mask % 2)
            int_mask = int_mask // 2
        if has_series(results):
            count_row_of_5 += 1
        count_heads.append(sum(results))
    average_heads = sum(count_heads) / len(count_heads)
    print(average_heads)
    probability_more_60 = sum([1 for x in count_heads if x > 60]) / len(count_heads) * 100
    print(probability_more_60, " больше 60")
    for _ in range(8):
        probability = sum([1 for x in count_heads if (x >= _ * 10) and (x < (_ + 1) * 10)]) / len(count_heads) * 100
        print(probability, f" [{_ * 10}, {(_ + 1) * 10})")
    probability = sum([1 for x in count_heads if (x >= 90) and (x <= 100)]) / len(count_heads) * 100
    print(probability, f" [90, 100]")
    lower = binom.ppf(alpha / 2, n, p)
    upper = binom.ppf(1 - alpha / 2, n, p)
    print(f"95% интервал: [{lower}, {upper}]")
    print(count_row_of_5 / 10 ** 4, "вероятность выпадения хотя бы одной серии из 5 орлов подряд в процентах")

def new():
    plt.style.use('seaborn-v0_8-darkgrid')
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(14, 5))

    p_values = np.linspace(0.01, 0.99, 100)
    n = 100
    alpha = 0.05

    expected = n * p_values
    ax1.plot(p_values, expected, 'b-', linewidth=2, label='E(X) = 100p')
    ax1.set_xlabel('Вероятность орла (p)', fontsize=12)
    ax1.set_ylabel('Ожидаемое число орлов', fontsize=12)
    ax1.set_title('Зависимость ожидаемого числа орлов от p', fontsize=14)
    ax1.grid(True, alpha=0.3)
    ax1.legend()
    ax1.set_xlim(0, 1)
    ax1.set_ylim(0, 100)

    sigma = np.sqrt(n * p_values * (1 - p_values))
    width_normal = 2 * 1.96 * sigma

    width_exact = []
    for p in p_values:
        lower = binom.ppf(alpha / 2, n, p)
        upper = binom.ppf(1 - alpha / 2, n, p)
        width_exact.append(upper - lower)

    ax2.plot(p_values, width_normal, 'r-', linewidth=2,
             label='Нормальное приближение: 2·1.96·σ')
    ax2.plot(p_values, width_exact, 'b--', linewidth=2,
             label='Точные биномиальные квантили')
    ax2.set_xlabel('Вероятность орла (p)', fontsize=12)
    ax2.set_ylabel('Ширина 95% интервала', fontsize=12)
    ax2.set_title('Зависимость ширины предсказательного интервала от p', fontsize=14)
    ax2.grid(True, alpha=0.3)
    ax2.legend()
    ax2.set_xlim(0, 1)
    ax2.set_ylim(0, 70)

    plt.tight_layout()
    plt.show()

    print("Примеры значений:")
    print("-" * 50)
    test_p = [0.1, 0.3, 0.5, 0.7, 0.9]
    for p in test_p:
        lower = binom.ppf(alpha / 2, n, p)
        upper = binom.ppf(1 - alpha / 2, n, p)
        width = upper - lower
        print(f"p = {p:.1f}: ожидание = {n * p:.1f}, интервал = [{lower:.0f}, {upper:.0f}], ширина = {width:.0f}")

    n = 100
    num_experiments = 50000
    p_values = np.linspace(0.01, 0.99, 30)

    prob_series = []
    avg_max_series = []

    for p in tqdm(p_values):
        series_count = 0
        max_series_sum = 0

        for _ in range(num_experiments):
            results = [1 if np.random.random() < p else 0 for _ in range(n)]

            if has_series(results):
                series_count += 1

            max_series_sum += max_series_length(results)

        prob_series.append(series_count / num_experiments)
        avg_max_series.append(max_series_sum / num_experiments)

    plt.style.use('seaborn-v0_8-darkgrid')
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(14, 5))

    ax1.plot(p_values, prob_series, 'g-', linewidth=2, marker='o', markersize=4)
    ax1.axhline(y=0.81, color='gray', linestyle='--', alpha=0.5, label='p=0.5')
    ax1.set_xlabel('Вероятность орла (p)')
    ax1.set_ylabel('Вероятность серии из 5 орлов')
    ax1.set_title('Зависимость вероятности серии из 5 орлов от p')
    ax1.grid(True, alpha=0.3)
    ax1.legend()
    ax1.set_xlim(0, 1)
    ax1.set_ylim(0, 1.05)

    ax2.plot(p_values, avg_max_series, 'm-', linewidth=2, marker='s', markersize=4)
    ax2.axhline(y=6.6, color='gray', linestyle='--', alpha=0.5, label='p=0.5')
    ax2.set_xlabel('Вероятность орла (p)')
    ax2.set_ylabel('Средняя длина максимальной серии')
    ax2.set_title('Зависимость средней длины максимальной серии от p')
    ax2.grid(True, alpha=0.3)
    ax2.legend()
    ax2.set_xlim(0, 1)
    ax2.set_ylim(0, n + 5)

    plt.tight_layout()
    plt.show()

default()
new()