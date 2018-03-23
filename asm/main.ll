declare void @.main()

define i32 @main() {
  call void @.main()
  ret i32 0
}

