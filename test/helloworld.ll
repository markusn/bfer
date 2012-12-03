declare void @llvm.memset.i32(i8* nocapture, i8, i32, i32) nounwind
declare i32 @getchar()
declare i32 @putchar(i32)
declare noalias i8* @malloc(i32) nounwind
declare void @free(i8*) nounwind
define void @main() {
main.0:
  %arr = call i8* @malloc(i32 65536)
  call void @llvm.memset.i32(i8* %arr, i8 0, i32 65536, i32 1)
  %head.0 = getelementptr i8* %arr, i32 32768
  %tape.1 = load i8* %head.0
  %tape.2 = add i8 %tape.1, 1
  store i8 %tape.2, i8* %head.0
  %tape.3 = load i8* %head.0
  %tape.4 = add i8 %tape.3, 1
  store i8 %tape.4, i8* %head.0
  %tape.5 = load i8* %head.0
  %tape.6 = add i8 %tape.5, 1
  store i8 %tape.6, i8* %head.0
  %tape.7 = load i8* %head.0
  %tape.8 = add i8 %tape.7, 1
  store i8 %tape.8, i8* %head.0
  %tape.9 = load i8* %head.0
  %tape.10 = add i8 %tape.9, 1
  store i8 %tape.10, i8* %head.0
  %tape.11 = load i8* %head.0
  %tape.12 = add i8 %tape.11, 1
  store i8 %tape.12, i8* %head.0
  %tape.13 = load i8* %head.0
  %tape.14 = add i8 %tape.13, 1
  store i8 %tape.14, i8* %head.0
  %tape.15 = load i8* %head.0
  %tape.16 = add i8 %tape.15, 1
  store i8 %tape.16, i8* %head.0
  %tape.17 = load i8* %head.0
  %tape.18 = add i8 %tape.17, 1
  store i8 %tape.18, i8* %head.0
  %tape.19 = load i8* %head.0
  %tape.20 = add i8 %tape.19, 1
  store i8 %tape.20, i8* %head.0
  br label %main.1 ; [
  main.2: ; loop-body
  %head.2 = getelementptr i8* %head.1, i32 1
  %tape.21 = load i8* %head.2
  %tape.22 = add i8 %tape.21, 1
  store i8 %tape.22, i8* %head.2
  %tape.23 = load i8* %head.2
  %tape.24 = add i8 %tape.23, 1
  store i8 %tape.24, i8* %head.2
  %tape.25 = load i8* %head.2
  %tape.26 = add i8 %tape.25, 1
  store i8 %tape.26, i8* %head.2
  %tape.27 = load i8* %head.2
  %tape.28 = add i8 %tape.27, 1
  store i8 %tape.28, i8* %head.2
  %tape.29 = load i8* %head.2
  %tape.30 = add i8 %tape.29, 1
  store i8 %tape.30, i8* %head.2
  %tape.31 = load i8* %head.2
  %tape.32 = add i8 %tape.31, 1
  store i8 %tape.32, i8* %head.2
  %tape.33 = load i8* %head.2
  %tape.34 = add i8 %tape.33, 1
  store i8 %tape.34, i8* %head.2
  %head.3 = getelementptr i8* %head.2, i32 1
  %tape.35 = load i8* %head.3
  %tape.36 = add i8 %tape.35, 1
  store i8 %tape.36, i8* %head.3
  %tape.37 = load i8* %head.3
  %tape.38 = add i8 %tape.37, 1
  store i8 %tape.38, i8* %head.3
  %tape.39 = load i8* %head.3
  %tape.40 = add i8 %tape.39, 1
  store i8 %tape.40, i8* %head.3
  %tape.41 = load i8* %head.3
  %tape.42 = add i8 %tape.41, 1
  store i8 %tape.42, i8* %head.3
  %tape.43 = load i8* %head.3
  %tape.44 = add i8 %tape.43, 1
  store i8 %tape.44, i8* %head.3
  %tape.45 = load i8* %head.3
  %tape.46 = add i8 %tape.45, 1
  store i8 %tape.46, i8* %head.3
  %tape.47 = load i8* %head.3
  %tape.48 = add i8 %tape.47, 1
  store i8 %tape.48, i8* %head.3
  %tape.49 = load i8* %head.3
  %tape.50 = add i8 %tape.49, 1
  store i8 %tape.50, i8* %head.3
  %tape.51 = load i8* %head.3
  %tape.52 = add i8 %tape.51, 1
  store i8 %tape.52, i8* %head.3
  %tape.53 = load i8* %head.3
  %tape.54 = add i8 %tape.53, 1
  store i8 %tape.54, i8* %head.3
  %head.4 = getelementptr i8* %head.3, i32 1
  %tape.55 = load i8* %head.4
  %tape.56 = add i8 %tape.55, 1
  store i8 %tape.56, i8* %head.4
  %tape.57 = load i8* %head.4
  %tape.58 = add i8 %tape.57, 1
  store i8 %tape.58, i8* %head.4
  %tape.59 = load i8* %head.4
  %tape.60 = add i8 %tape.59, 1
  store i8 %tape.60, i8* %head.4
  %head.5 = getelementptr i8* %head.4, i32 1
  %tape.61 = load i8* %head.5
  %tape.62 = add i8 %tape.61, 1
  store i8 %tape.62, i8* %head.5
  %head.6 = getelementptr i8* %head.5, i32 -1
  %head.7 = getelementptr i8* %head.6, i32 -1
  %head.8 = getelementptr i8* %head.7, i32 -1
  %head.9 = getelementptr i8* %head.8, i32 -1
  %tape.63 = load i8* %head.9
  %tape.64 = add i8 %tape.63, -1
  store i8 %tape.64, i8* %head.9
  br label %main.1 ; ]
  main.1: ; loop-test
  %head.1 = phi i8* [%head.0, %main.0], [%head.9, %main.2]
  %tape.65 = load i8* %head.1
  %test.1 = icmp eq i8 %tape.65, 0
  br i1 %test.1, label %main.3, label %main.2
  main.3: ; loop-after
  %head.10 = phi i8* [%head.1, %main.1]
  %head.11 = getelementptr i8* %head.10, i32 1
  %tape.66 = load i8* %head.11
  %tape.67 = add i8 %tape.66, 1
  store i8 %tape.67, i8* %head.11
  %tape.68 = load i8* %head.11
  %tape.69 = add i8 %tape.68, 1
  store i8 %tape.69, i8* %head.11
  %tape.70 = load i8* %head.11
  %tape.71 = sext i8 %tape.70 to i32
  call i32 @putchar(i32 %tape.71)
  %head.12 = getelementptr i8* %head.11, i32 1
  %tape.72 = load i8* %head.12
  %tape.73 = add i8 %tape.72, 1
  store i8 %tape.73, i8* %head.12
  %tape.74 = load i8* %head.12
  %tape.75 = sext i8 %tape.74 to i32
  call i32 @putchar(i32 %tape.75)
  %tape.76 = load i8* %head.12
  %tape.77 = add i8 %tape.76, 1
  store i8 %tape.77, i8* %head.12
  %tape.78 = load i8* %head.12
  %tape.79 = add i8 %tape.78, 1
  store i8 %tape.79, i8* %head.12
  %tape.80 = load i8* %head.12
  %tape.81 = add i8 %tape.80, 1
  store i8 %tape.81, i8* %head.12
  %tape.82 = load i8* %head.12
  %tape.83 = add i8 %tape.82, 1
  store i8 %tape.83, i8* %head.12
  %tape.84 = load i8* %head.12
  %tape.85 = add i8 %tape.84, 1
  store i8 %tape.85, i8* %head.12
  %tape.86 = load i8* %head.12
  %tape.87 = add i8 %tape.86, 1
  store i8 %tape.87, i8* %head.12
  %tape.88 = load i8* %head.12
  %tape.89 = add i8 %tape.88, 1
  store i8 %tape.89, i8* %head.12
  %tape.90 = load i8* %head.12
  %tape.91 = sext i8 %tape.90 to i32
  call i32 @putchar(i32 %tape.91)
  %tape.92 = load i8* %head.12
  %tape.93 = sext i8 %tape.92 to i32
  call i32 @putchar(i32 %tape.93)
  %tape.94 = load i8* %head.12
  %tape.95 = add i8 %tape.94, 1
  store i8 %tape.95, i8* %head.12
  %tape.96 = load i8* %head.12
  %tape.97 = add i8 %tape.96, 1
  store i8 %tape.97, i8* %head.12
  %tape.98 = load i8* %head.12
  %tape.99 = add i8 %tape.98, 1
  store i8 %tape.99, i8* %head.12
  %tape.100 = load i8* %head.12
  %tape.101 = sext i8 %tape.100 to i32
  call i32 @putchar(i32 %tape.101)
  %head.13 = getelementptr i8* %head.12, i32 1
  %tape.102 = load i8* %head.13
  %tape.103 = add i8 %tape.102, 1
  store i8 %tape.103, i8* %head.13
  %tape.104 = load i8* %head.13
  %tape.105 = add i8 %tape.104, 1
  store i8 %tape.105, i8* %head.13
  %tape.106 = load i8* %head.13
  %tape.107 = sext i8 %tape.106 to i32
  call i32 @putchar(i32 %tape.107)
  %head.14 = getelementptr i8* %head.13, i32 -1
  %head.15 = getelementptr i8* %head.14, i32 -1
  %tape.108 = load i8* %head.15
  %tape.109 = add i8 %tape.108, 1
  store i8 %tape.109, i8* %head.15
  %tape.110 = load i8* %head.15
  %tape.111 = add i8 %tape.110, 1
  store i8 %tape.111, i8* %head.15
  %tape.112 = load i8* %head.15
  %tape.113 = add i8 %tape.112, 1
  store i8 %tape.113, i8* %head.15
  %tape.114 = load i8* %head.15
  %tape.115 = add i8 %tape.114, 1
  store i8 %tape.115, i8* %head.15
  %tape.116 = load i8* %head.15
  %tape.117 = add i8 %tape.116, 1
  store i8 %tape.117, i8* %head.15
  %tape.118 = load i8* %head.15
  %tape.119 = add i8 %tape.118, 1
  store i8 %tape.119, i8* %head.15
  %tape.120 = load i8* %head.15
  %tape.121 = add i8 %tape.120, 1
  store i8 %tape.121, i8* %head.15
  %tape.122 = load i8* %head.15
  %tape.123 = add i8 %tape.122, 1
  store i8 %tape.123, i8* %head.15
  %tape.124 = load i8* %head.15
  %tape.125 = add i8 %tape.124, 1
  store i8 %tape.125, i8* %head.15
  %tape.126 = load i8* %head.15
  %tape.127 = add i8 %tape.126, 1
  store i8 %tape.127, i8* %head.15
  %tape.128 = load i8* %head.15
  %tape.129 = add i8 %tape.128, 1
  store i8 %tape.129, i8* %head.15
  %tape.130 = load i8* %head.15
  %tape.131 = add i8 %tape.130, 1
  store i8 %tape.131, i8* %head.15
  %tape.132 = load i8* %head.15
  %tape.133 = add i8 %tape.132, 1
  store i8 %tape.133, i8* %head.15
  %tape.134 = load i8* %head.15
  %tape.135 = add i8 %tape.134, 1
  store i8 %tape.135, i8* %head.15
  %tape.136 = load i8* %head.15
  %tape.137 = add i8 %tape.136, 1
  store i8 %tape.137, i8* %head.15
  %tape.138 = load i8* %head.15
  %tape.139 = sext i8 %tape.138 to i32
  call i32 @putchar(i32 %tape.139)
  %head.16 = getelementptr i8* %head.15, i32 1
  %tape.140 = load i8* %head.16
  %tape.141 = sext i8 %tape.140 to i32
  call i32 @putchar(i32 %tape.141)
  %tape.142 = load i8* %head.16
  %tape.143 = add i8 %tape.142, 1
  store i8 %tape.143, i8* %head.16
  %tape.144 = load i8* %head.16
  %tape.145 = add i8 %tape.144, 1
  store i8 %tape.145, i8* %head.16
  %tape.146 = load i8* %head.16
  %tape.147 = add i8 %tape.146, 1
  store i8 %tape.147, i8* %head.16
  %tape.148 = load i8* %head.16
  %tape.149 = sext i8 %tape.148 to i32
  call i32 @putchar(i32 %tape.149)
  %tape.150 = load i8* %head.16
  %tape.151 = add i8 %tape.150, -1
  store i8 %tape.151, i8* %head.16
  %tape.152 = load i8* %head.16
  %tape.153 = add i8 %tape.152, -1
  store i8 %tape.153, i8* %head.16
  %tape.154 = load i8* %head.16
  %tape.155 = add i8 %tape.154, -1
  store i8 %tape.155, i8* %head.16
  %tape.156 = load i8* %head.16
  %tape.157 = add i8 %tape.156, -1
  store i8 %tape.157, i8* %head.16
  %tape.158 = load i8* %head.16
  %tape.159 = add i8 %tape.158, -1
  store i8 %tape.159, i8* %head.16
  %tape.160 = load i8* %head.16
  %tape.161 = add i8 %tape.160, -1
  store i8 %tape.161, i8* %head.16
  %tape.162 = load i8* %head.16
  %tape.163 = sext i8 %tape.162 to i32
  call i32 @putchar(i32 %tape.163)
  %tape.164 = load i8* %head.16
  %tape.165 = add i8 %tape.164, -1
  store i8 %tape.165, i8* %head.16
  %tape.166 = load i8* %head.16
  %tape.167 = add i8 %tape.166, -1
  store i8 %tape.167, i8* %head.16
  %tape.168 = load i8* %head.16
  %tape.169 = add i8 %tape.168, -1
  store i8 %tape.169, i8* %head.16
  %tape.170 = load i8* %head.16
  %tape.171 = add i8 %tape.170, -1
  store i8 %tape.171, i8* %head.16
  %tape.172 = load i8* %head.16
  %tape.173 = add i8 %tape.172, -1
  store i8 %tape.173, i8* %head.16
  %tape.174 = load i8* %head.16
  %tape.175 = add i8 %tape.174, -1
  store i8 %tape.175, i8* %head.16
  %tape.176 = load i8* %head.16
  %tape.177 = add i8 %tape.176, -1
  store i8 %tape.177, i8* %head.16
  %tape.178 = load i8* %head.16
  %tape.179 = add i8 %tape.178, -1
  store i8 %tape.179, i8* %head.16
  %tape.180 = load i8* %head.16
  %tape.181 = sext i8 %tape.180 to i32
  call i32 @putchar(i32 %tape.181)
  %head.17 = getelementptr i8* %head.16, i32 1
  %tape.182 = load i8* %head.17
  %tape.183 = add i8 %tape.182, 1
  store i8 %tape.183, i8* %head.17
  %tape.184 = load i8* %head.17
  %tape.185 = sext i8 %tape.184 to i32
  call i32 @putchar(i32 %tape.185)
  %head.18 = getelementptr i8* %head.17, i32 1
  %tape.186 = load i8* %head.18
  %tape.187 = sext i8 %tape.186 to i32
  call i32 @putchar(i32 %tape.187)
  call void @free(i8* %arr)
  ret void
}
