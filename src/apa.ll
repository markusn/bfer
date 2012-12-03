
define i32 @main() nounwind uwtable {
  %1 = alloca i32, align 4
  %array = alloca [30000 x i8], align 16
  store i32 0, i32* %1
  %2 = getelementptr inbounds [30000 x i8]* %array, i32 0, i64 5
  %3 = load i8* %2, align 1
  %4 = sext i8 %3 to i32
  %5 = add nsw i32 %4, 1
  %6 = trunc i32 %5 to i8
  store i8 %6, i8* %2, align 1
  %7 = getelementptr inbounds [30000 x i8]* %array, i32 0, i64 4
  %8 = load i8* %7, align 1
  %9 = sext i8 %8 to i32
  %10 = call i32 @putchar(i32 %9)
  %11 = getelementptr inbounds [30000 x i8]* %array, i32 0, i64 5
  %12 = load i8* %11, align 1
  %13 = sext i8 %12 to i32
  %14 = call i32 @putchar(i32 %13)
  ret i32 0
}

declare i32 @putchar(i32)