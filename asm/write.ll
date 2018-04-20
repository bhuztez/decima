declare void @write(i32, i8*, i64)

define void @std.io.write(i32* %fd, i8** %buf, i64* %size) {
  %1 = load i32, i32* %fd
  %2 = load i8*, i8** %buf
  %3 = load i64, i64* %size
  call void @write(i32 %1, i8* %2, i64 %3)
  ret void
}
