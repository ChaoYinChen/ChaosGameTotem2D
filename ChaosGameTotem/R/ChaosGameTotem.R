#'ChaosGameTotem
#'
#'進行Chaos Game，依照package使用者在平面上所設置的初始點與隨機的頂點及所走的步長，迭代數次後點出一個圖騰。
#'
#'Chaos Game是依照玩家給予在平面上一個初始點與任意三個(含)以上的頂點及步長比例。利用隨機選擇玩家給予的任一個頂點，並將其與初始點的距離依照步長比例，畫出一個點，此點即為下一個起點。反覆這些步驟，迭代多次即可畫出明顯的圖騰。
#'
#'
#' @param initial 在平面上任意一個初始點及三個(含)以上頂點，為矩陣型態。
#' @param q 步長比例，設定於[-0.5,0.5]範圍裡，為數值型態。
#' @param iteration 迭代次數，預設=20000。
#'
#'
#' @export
#'
#' @examples
#' initial = matrix(c(2,0,55,80,1,-1,100,25),nrow = 4,ncol = 2,byrow = TRUE)
#' #初始點(2,0)、三個頂點(55,80)、(1,-1)、(100,25)
#' game(initial,q=0.5)
#' # returns 一個三角形圖騰
#'
#' @examples
#' initial = matrix(c(2,0,55,80,1,-1,100,25,-10,-20),nrow = 4,ncol = 2,byrow = TRUE)
#' #初始點(2,0)、四個頂點(55,80)、(1,-1)、(100,25)、(-10,-20)
#' game(initial,q=-0.3,iteration = 1000)
#' # returns 一個迭代1000次的四角圖騰

game = function(initial,q,iteration = 20000){

  dim = 2
  iteration = iteration + 1
  j = dim(initial)[1]

  points = matrix(0,nrow = iteration,ncol = dim)
  vertex = sample(2:j,size = iteration, replace =TRUE)

  points[1,] = initial[1,]

  for(i in 1: (iteration-1)){
    startpoint = initial[vertex[i+1],]
    points[i+1,] = (1-q)*startpoint + q*points[i,]
  }
  plot(points[,1],points[,2])
  points(initial[,1],initial[,2],col=2,pch=16)
}
