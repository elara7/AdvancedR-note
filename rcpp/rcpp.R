library(Rcpp)

cppFunction(
  'int add(int x, int y, int z){
  int sum = x + y + z;
  return sum;
  }'
)
add
add(3, 4, 5)
# remark
# c++代码用字符串，c++代码需要;分割

# 没有输入，标量输出

cppFunction('
            int one(){
            return 1L;
            }'
)
one()

# 标量输入，标量输出