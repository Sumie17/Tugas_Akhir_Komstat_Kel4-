AnovaLab dikembangkan menggunakan R Shiny dan shinydashboard sebagai media interaktif untuk melakukan analisis ANOVA satu arah.
Pengguna dapat mengunggah data dari file Excel dan menjalankan tahapan analisis statistik secara runtut, mulai dari uji asumsi hingga uji lanjutan, tanpa perlu menulis kode R secara manual.
Aplikasi ini dirancang untuk digunakan dalam konteks pembelajaran, penelitian, maupun analisis praktis di bidang statistik.

🧪 Analisis yang Didukung
~ Tahapan analisis dalam aplikasi meliputi:
~ Uji Kenormalan (Shapiro-Wilk) terhadap seluruh nilai numerik
~ Uji Homogenitas (Bartlett Test) berdasarkan pembagian grup
~ ANOVA satu arah untuk mengetahui perbedaan antar grup
~ Uji Tukey HSD sebagai analisis pasca ANOVA

✅ Fitur-Fitur Aplikasi
= Unggah file Excel (.xlsx / .xls) sebagai input data
= Pilih sheet dari file Excel secara dinamis
= Baca data dengan dua kolom utama: grup (faktor) dan nilai (numerik)
= Tampilkan cuplikan data (preview) sebelum dianalisis
= Lakukan uji kenormalan (Shapiro-Wilk) untuk keseluruhan data
= Jalankan uji homogenitas varians (Bartlett) antar grup
= Hitung dan tampilkan hasil ANOVA satu arah
= Sajikan hasil uji Tukey HSD jika ANOVA signifikan
= Output ditampilkan secara verbatim dan disertai indikator interpretasi (warna hijau/merah)
= Navigasi tab yang terstruktur: dari Input Data → Uji Asumsi → ANOVA → Tukey
