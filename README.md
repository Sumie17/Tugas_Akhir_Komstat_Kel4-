# 🧪 AnovaLab

**AnovaLab** dikembangkan menggunakan `R Shiny` dan `shinydashboard` sebagai media interaktif untuk melakukan analisis **ANOVA satu arah**.

Pengguna dapat mengunggah data dari file Excel dan menjalankan tahapan analisis statistik secara runtut, mulai dari uji asumsi hingga uji lanjutan, **tanpa perlu menulis kode R secara manual**.

Aplikasi ini dirancang untuk digunakan dalam konteks:
- 🎓 Pembelajaran
- 🔬 Penelitian
- 📊 Analisis praktis di bidang statistik

---

## ⚙️ Tahapan Analisis yang Didukung

AnovaLab memandu pengguna melalui tahapan analisis statistik berikut:

1. ✅ **Uji Kenormalan**  
   Menggunakan *Shapiro-Wilk test* terhadap seluruh nilai numerik

2. ✅ **Uji Homogenitas Varians**  
   Menggunakan *Bartlett test* berdasarkan pembagian grup

3. ✅ **ANOVA Satu Arah**  
   Untuk mengetahui apakah terdapat perbedaan signifikan antar grup

4. ✅ **Uji Tukey HSD**  
   Sebagai analisis lanjutan jika ANOVA menunjukkan hasil signifikan

---

## 📌 Fitur-Fitur Aplikasi

- 📂 **Unggah file Excel** (`.xlsx` / `.xls`) sebagai input data  
- 📑 **Pilih sheet** dari file Excel secara dinamis  
- 🧾 **Baca data** dengan dua kolom utama:
  - `grup` (sebagai faktor)
  - `nilai` (sebagai variabel numerik)
- 👁️‍🗨️ **Tampilkan cuplikan data** (*preview*) sebelum analisis  
- 🔍 **Lakukan uji kenormalan** (*Shapiro-Wilk*) untuk seluruh data  
- ⚖️ **Lakukan uji homogenitas** varians antar grup (*Bartlett test*)  
- 📊 **Hitung dan tampilkan hasil ANOVA satu arah**  
- 🧪 **Sajikan hasil uji Tukey HSD** jika ANOVA signifikan  
- 🖥️ **Output ditampilkan secara verbatim**, dilengkapi indikator interpretasi:
  - 🟢 Hijau = signifikan
  - 🔴 Merah = tidak signifikan  
- 🧭 **Navigasi tab terstruktur**:
  - `Input Data` → `Uji Asumsi` → `ANOVA` → `Tukey`

---

## 🚀 Teknologi

Aplikasi ini dibangun menggunakan:
- `R`
- `shiny`
- `shinydashboard`
- `readxl`
- `ggplot2`
- `dplyr`
- dan library pendukung lainnya

---

## 🎉 Selamat Mencoba!

Jika kamu menemukan bug atau ingin mengembangkan lebih lanjut, silakan ajukan **issue** atau buat **pull request** ke repositori ini.
