class players(var Year: Int, var PlayerName: String, var Country: String, var Matches: Int, var Runs: Int, var Wickets: Int) {}

object players {

  def Desc[T: Ordering] = implicitly[Ordering[T]].reverse;

  def push_data(Year: Int, PlayerName: String, Country: String, Matches: Int, Runs: Int, Wickets: Int): players = {
    val playerStat = new players(Year, PlayerName, Country, Matches, Runs, Wickets);
    playerStat;
  }

  def trim(value: Double, precision: Int): Double = {
    val x = math.pow(10, precision)
    (math floor value * x) / x
  }

  def main(args: Array[String]): Unit = {

    val bufferedSource = scala.io.Source.fromFile("/Users/shashankdey/IdeaProjects/HelloWorld/src/data.csv")
    var playerList = List(push_data(2020, "Williamson", "New Zealand", 24, 2558, 4))

    for (element <- bufferedSource.getLines) {
      val columns = element.split(",").map(_.trim)
      playerList = playerList :+ push_data(columns(0).toInt, columns(1), columns(2), columns(3).toInt, columns(4).toInt, columns(5).toInt)
    }

    println("====================================================================================================")

    println("1. Player with the best highest runs scored.");
    val player = playerList.sortBy(x => x.Runs).reverse
    println(s"Player Name: ${player(0).PlayerName} | Country: ${player(0).Country} |Runs scored: ${player(0).Runs}")

    println("====================================================================================================")

    println("2. Top 5 players by runs scored.")
    for (player <- player.take(5)){
      println(s"Player Name: ${player.PlayerName} | Country: ${player.Country} | Runs scored: ${player.Runs}")
    }

    println("====================================================================================================")

    println("3. Top 5 players by wicket taken.")
    val player1 = playerList.sortBy(x => x.Wickets).reverse
    for (player <- player1.take(5)) {
      println(s"Player Name: ${player.PlayerName} | Country: ${player.Country} | Wickets taken: ${player.Wickets}")
    }

    println("====================================================================================================")

    println("4. Rank players with overall performance give weight 5x to wicket taken and (5/100)x to run scored.")
    playerList = playerList.sortBy(x => x.Wickets * 5 + x.Runs * 0.05).reverse
    var rank: Int = 1
    for (player <- playerList) {
      println(s"Rank $rank: ${player.PlayerName} | Rating: ${trim(player.Wickets * 5 + player.Runs * 0.05, 3)}")
      rank = rank + 1
    }

    println("====================================================================================================")
  }
}



