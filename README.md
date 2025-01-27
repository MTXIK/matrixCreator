# Программа для работы с массивами в EMU8086

## Описание

Этот репозиторий содержит программу на языке ассемблера, предназначенную для работы с одномерными массивами. Код предназначен для выполнения в эмуляторе **EMU8086**.

## Содержание
- **Задание**: Разработка программы для ввода двух одномерных массивов A и B и вычисления значений матрицы C по заданной формуле.
- **Блок-схема**: Графическое представление алгоритма.
- **Ассемблерный код**: Программа на языке ассемблера, реализующая расчет матрицы.
- **Проверочный код на C++**: Программа на C++, предназначенная для проверки корректности вычислений.
- **Результаты выполнения программы**: Скриншоты работы программ на обоих языках.

## Структура файлов
- `assembly_code.asm` - исходный код программы на ассемблере.
- `verification.cpp` - проверочный код на C++.
- `report.docx` - отчет с результатами выполнения программы.

## Использование

### Запуск программы в EMU8086

1. Открыть **EMU8086** и загрузить файл `assembly_code.asm`.
2. Собрать программу и запустить эмуляцию.

### Запуск проверочного кода на C++

1. Компилируйте программу с использованием `g++`:
   ```sh
   g++ verification.cpp -o verification
