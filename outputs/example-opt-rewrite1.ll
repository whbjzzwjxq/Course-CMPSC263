target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

define dso_local i32 @_Z11symbolicI32v() #0 {
entry:
  ret i32 0
}

define dso_local i32 @main() #1 {
entry:
  %call = call i32 @_Z11symbolicI32v()
  %call1 = call i32 @_Z11symbolicI32v()
  %cmp = icmp sle i32 %call, 128
  br i1 %cmp, label %if.then, label %if.else

if.then:                                          ; preds = %entry
  %mul = mul nsw i32 %call, %call
  %mul2 = mul nsw i32 %call1, %call1
  %mul3 = mul nsw i32 2, %call1
  %mul4 = mul nsw i32 %mul3, %call
  %add = add nsw i32 %mul, %mul4
  %add5 = add nsw i32 %add, %mul2
  br label %if.end

if.else:                                          ; preds = %entry
  br label %if.end

if.end:                                           ; preds = %if.else, %if.then
  %z.0 = phi i32 [ %add5, %if.then ], [ 0, %if.else ]
  ret i32 %z.0
}

attributes #0 = { mustprogress noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { mustprogress noinline norecurse nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }

!llvm.module.flags = !{!0, !1, !2}
!llvm.ident = !{!3}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"uwtable", i32 1}
!2 = !{i32 7, !"frame-pointer", i32 2}
!3 = !{!"clang version 13.0.1 (git@github.com:llvm/llvm-project.git 75e33f71c2dae584b13a7d1186ae0a038ba98838)"}
