declare void @llvm.memset.p0i8.i32(i8* nocapture, i8, i32, i32, i1) nounwind
declare i32 @getchar()
declare i32 @putchar(i32)
declare noalias i8* @malloc(i32) nounwind
declare void @free(i8*) nounwind
define void @main() {
  main.0:
    %arr = call i8* @malloc(i32 65536)
    call void @llvm.memset.p0i8.i32(i8* %arr, i8 0, i32 65536, i32 1, i1 false)
    %head.0 = getelementptr i8* %arr, i32 32768
    ; add/sub
    %tape.1 = load i8* %head.0
    %tape.2 = add i8 %tape.1, 10
    store i8 %tape.2, i8* %head.0
    br label %main.1 ; [
    main.2: ; loop-body
      ; left/right
      %head.2 = getelementptr i8* %head.1, i32 1
      ; add/sub
      %tape.3 = load i8* %head.2
      %tape.4 = add i8 %tape.3, 7
      store i8 %tape.4, i8* %head.2
      ; left/right
      %head.3 = getelementptr i8* %head.2, i32 1
      ; add/sub
      %tape.5 = load i8* %head.3
      %tape.6 = add i8 %tape.5, 10
      store i8 %tape.6, i8* %head.3
      ; left/right
      %head.4 = getelementptr i8* %head.3, i32 1
      ; add/sub
      %tape.7 = load i8* %head.4
      %tape.8 = add i8 %tape.7, 3
      store i8 %tape.8, i8* %head.4
      ; left/right
      %head.5 = getelementptr i8* %head.4, i32 1
      ; add/sub
      %tape.9 = load i8* %head.5
      %tape.10 = add i8 %tape.9, 1
      store i8 %tape.10, i8* %head.5
      ; left/right
      %head.6 = getelementptr i8* %head.5, i32 -4
      ; add/sub
      %tape.11 = load i8* %head.6
      %tape.12 = add i8 %tape.11, -1
      store i8 %tape.12, i8* %head.6
      br label %main.1 ; ]
    main.1: ; loop-test
      %head.1 = phi i8* [%head.0, %main.0], [%head.6, %main.2]
      %tape.13 = load i8* %head.1
      %test.1 = icmp eq i8 %tape.13, 0
      br i1 %test.1, label %main.3, label %main.2
    main.3: ; loop-after
      %head.7 = phi i8* [%head.1, %main.1]
      ; left/right
      %head.8 = getelementptr i8* %head.7, i32 1
      ; add/sub
      %tape.14 = load i8* %head.8
      %tape.15 = add i8 %tape.14, 2
      store i8 %tape.15, i8* %head.8
      ; put
      %tape.16 = load i8* %head.8
      %tape.17 = sext i8 %tape.16 to i32
      call i32 @putchar(i32 %tape.17)
      ; left/right
      %head.9 = getelementptr i8* %head.8, i32 1
      ; add/sub
      %tape.18 = load i8* %head.9
      %tape.19 = add i8 %tape.18, 1
      store i8 %tape.19, i8* %head.9
      ; put
      %tape.20 = load i8* %head.9
      %tape.21 = sext i8 %tape.20 to i32
      call i32 @putchar(i32 %tape.21)
      ; add/sub
      %tape.22 = load i8* %head.9
      %tape.23 = add i8 %tape.22, 7
      store i8 %tape.23, i8* %head.9
      ; put
      %tape.24 = load i8* %head.9
      %tape.25 = sext i8 %tape.24 to i32
      call i32 @putchar(i32 %tape.25)
      ; put
      %tape.26 = load i8* %head.9
      %tape.27 = sext i8 %tape.26 to i32
      call i32 @putchar(i32 %tape.27)
      ; add/sub
      %tape.28 = load i8* %head.9
      %tape.29 = add i8 %tape.28, 3
      store i8 %tape.29, i8* %head.9
      ; put
      %tape.30 = load i8* %head.9
      %tape.31 = sext i8 %tape.30 to i32
      call i32 @putchar(i32 %tape.31)
      ; left/right
      %head.10 = getelementptr i8* %head.9, i32 1
      ; add/sub
      %tape.32 = load i8* %head.10
      %tape.33 = add i8 %tape.32, 2
      store i8 %tape.33, i8* %head.10
      ; put
      %tape.34 = load i8* %head.10
      %tape.35 = sext i8 %tape.34 to i32
      call i32 @putchar(i32 %tape.35)
      ; left/right
      %head.11 = getelementptr i8* %head.10, i32 -2
      ; add/sub
      %tape.36 = load i8* %head.11
      %tape.37 = add i8 %tape.36, 15
      store i8 %tape.37, i8* %head.11
      ; put
      %tape.38 = load i8* %head.11
      %tape.39 = sext i8 %tape.38 to i32
      call i32 @putchar(i32 %tape.39)
      ; left/right
      %head.12 = getelementptr i8* %head.11, i32 1
      ; put
      %tape.40 = load i8* %head.12
      %tape.41 = sext i8 %tape.40 to i32
      call i32 @putchar(i32 %tape.41)
      ; add/sub
      %tape.42 = load i8* %head.12
      %tape.43 = add i8 %tape.42, 3
      store i8 %tape.43, i8* %head.12
      ; put
      %tape.44 = load i8* %head.12
      %tape.45 = sext i8 %tape.44 to i32
      call i32 @putchar(i32 %tape.45)
      ; add/sub
      %tape.46 = load i8* %head.12
      %tape.47 = add i8 %tape.46, -6
      store i8 %tape.47, i8* %head.12
      ; put
      %tape.48 = load i8* %head.12
      %tape.49 = sext i8 %tape.48 to i32
      call i32 @putchar(i32 %tape.49)
      ; add/sub
      %tape.50 = load i8* %head.12
      %tape.51 = add i8 %tape.50, -8
      store i8 %tape.51, i8* %head.12
      ; put
      %tape.52 = load i8* %head.12
      %tape.53 = sext i8 %tape.52 to i32
      call i32 @putchar(i32 %tape.53)
      ; left/right
      %head.13 = getelementptr i8* %head.12, i32 1
      ; add/sub
      %tape.54 = load i8* %head.13
      %tape.55 = add i8 %tape.54, 1
      store i8 %tape.55, i8* %head.13
      ; put
      %tape.56 = load i8* %head.13
      %tape.57 = sext i8 %tape.56 to i32
      call i32 @putchar(i32 %tape.57)
      ; left/right
      %head.14 = getelementptr i8* %head.13, i32 1
      ; put
      %tape.58 = load i8* %head.14
      %tape.59 = sext i8 %tape.58 to i32
      call i32 @putchar(i32 %tape.59)
      call void @free(i8* %arr)
      ret void
}
