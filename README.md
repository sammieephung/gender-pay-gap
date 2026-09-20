# Bất bình đẳng thu nhập theo giới tính (Gender pay gap)
My first data analysis project in R as part of coursework for HUSO2313: Analysis & Communication of Social Science Research at RMIT University.

## Cấu trúc Repository
```bash
gender-pay-gap
├── data
│   ├── gender-pay-gap-codebook.docx
│   ├── pay_cleanv1.RData
│   └── pay_raw.xlsx
├── docs
│   ├── index.html
│   ├── part1.html
│   ├── part2.html
│   ├── part3.html
│   ├── search.json
│   └── site_libs
│       ├── bootstrap
│       │   ├── bootstrap-a0f9fd0d421825b736dc1bbcbcba2219.min.css
│       │   ├── bootstrap-icons.css
│       │   ├── bootstrap-icons.woff
│       │   └── bootstrap.min.js
│       ├── clipboard
│       │   └── clipboard.min.js
│       ├── quarto-html
│       │   ├── anchor.min.js
│       │   ├── popper.min.js
│       │   ├── quarto-syntax-highlighting-15634bcf2e68342d4ad2dfa704d543f6.css
│       │   ├── quarto.js
│       │   ├── tabsets
│       │   │   └── tabsets.js
│       │   ├── tippy.css
│       │   └── tippy.umd.min.js
│       ├── quarto-nav
│       │   ├── headroom.min.js
│       │   └── quarto-nav.js
│       └── quarto-search
│           ├── autocomplete.umd.js
│           ├── fuse.min.js
│           └── quarto-search.js
├── figures
├── gender-pay-gap.Rproj
├── index.qmd
├── part1.qmd
├── part2.qmd
├── part3.qmd
├── README.html
├── README.md
├── README_files
│   └── libs
│       ├── bootstrap
│       │   ├── bootstrap-efbb37e9afcc02144ebbc7afd12a776f.min.css
│       │   ├── bootstrap-icons.css
│       │   ├── bootstrap-icons.woff
│       │   └── bootstrap.min.js
│       ├── clipboard
│       │   └── clipboard.min.js
│       └── quarto-html
│           ├── anchor.min.js
│           ├── popper.min.js
│           ├── quarto-syntax-highlighting-15634bcf2e68342d4ad2dfa704d543f6.css
│           ├── quarto.js
│           ├── tabsets
│           │   └── tabsets.js
│           ├── tippy.css
│           └── tippy.umd.min.js
├── Rscripts
│   ├── data_cleaning.R
│   ├── data_modelling.R
│   ├── data_viz.R
│   └── LesaHoffman_Functions.R
└── _quarto.yml
```

## Về các Biến trong Data set (Data Dictionary)
| Tên biến | Loại biến | Mô tả                                         |
|---------------|---------------|-----------------------------------------------------|
| JobTitle<br>(*Vị trí*) | Phân loại | Vị trí làm việc của người điền khảo sát, bao gồm 10 loại: Manager, Driver, Data Scientist, Software Engineer, IT, Sales Associate, Graphic Designer, Warehouse Associate, Marketing Associate, Financial Analyst. |
| Gender<br>(*Giới tính*) | Phân loại | Giới tính theo hệ nhị nguyên của người điền khảo sát, bao gồm 2 loại: Nam (Male) và Nữ (Female) |
| Education <br>(*Học vấn*) | Phân loại | Trình độ học vấn cao nhất của người điền khảo sát, theo thứ tự thấp đến cao: High School (Trung học), College (Cử nhân Đại học), Masters (Thạc sĩ), PhD (Tiến sĩ). |
| Dept<br>(*Phòng ban*) | Phân loại | Phòng ban làm việc của người điền khảo sát, bao gồm: Administration (Hành chính), Management (Quản lí), Operations (Vận hành), Engineering (Kĩ thuật) và Sales (Kinh doanh). |
| Seniority<br>(*Thâm niên*) | Phân loại | Thứ bậc thâm niên của người điền khảo sát, trong đó 1 thấp nhất và 5 cao nhất. |
| BasePay<br>(*Lương cơ bản*) | Liên tục | Lương cơ bản của người điền khảo sát, đơn vị USD. |
| Bonus<br>(*Lương thưởng*) | Liên tục | Lương thưởng của người điền khảo sát, đơn vị USD. |
| Age<br>(*Tuổi*)| Liên tục | Tuổi của người điền khảo sát, đơn vị năm. |
| PerfEval<br>(*Đánh giá<br>hiệu suất*) | Phân loại | Thứ bậc xếp hạng hiệu suất làm việc của người điền khảo sát, trong đó 1 chỉ bậc hiệu suất thấp nhất và 5 chỉ bậc hiệu suất cao nhất. |


## Mô tả các file 

| Tên file | Mô tả | 
|-------------------|-----------------------------------------------------------------|
| pay_raw.xlsx      | Original data set in Excel format, containing 1,000  observations as provided by Kaggle                         |
| pay_cleanv1.RData | First version of the clean data set in .RData format, with 989 observations and Vietnamese labels for variables |