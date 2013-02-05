package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) => move match {
        case Left => block.left
        case Right => block.right
        case Up => block.up
        case Down => block.down
      }
    }
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/


    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  trait SimpleLevel extends SolutionChecker {
    /* terrain for simple level*/

   val level =
      """ooo---
        |oSoooo
        |ooooTo
        |-ooooo""".stripMargin

    val optsolution = List(Right, Down, Right)
  }


  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(!terrain(Pos(4,11)), "4,11")
      assert(terrain(Pos(1,1)), "1,1")
      assert(terrain(Pos(4,7)), "4,7")
      assert(!terrain(Pos(6,1)), "6,1")
    }
  }

  test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1,1))
      assert(goal == Pos(4,7))
      assert(findChar('A', Vector(Vector('o','o'),Vector('-','o'))) == Pos(-1,-1))
    }
  }

  test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }

  test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }



  test("valid column") {
    new Level1 {
      assert(validColumn(Vector('o','o','-','-'),1) == true)
      assert(validColumn(Vector('S','-','-','-'),0) == true)
      assert(validColumn(Vector('-','-','T','-'),2) == true)
      assert(validColumn(Vector('-','o','-'),0) == false)
      assert(validColumn(Vector('o','o'),2) == false)
      assert(validColumn(Vector(),0) == false)
      assert(validColumn(Vector(),2) == false)
      assert(validColumn(Vector('o','o'),-1) == false)
    }
  }

  test("isStanding") {
    new Level1 {
      val position1 = startPos
      val position2 = startPos
      val testStartBlock: Block = new Block(position1, position2)
      assert(testStartBlock.isStanding == true)
      assert(testStartBlock.down.isStanding == false)

    }
  }

  test("isLegal") {
    new Level1 {
      val position1 = startPos
      val position2 = startPos
      val testStartBlock: Block = new Block(position1, position2)
      assert(testStartBlock.isLegal == true)
      assert(testStartBlock.down.isLegal == true)

    }
  }


  test("neighbors") {
    new Level1 {

      assert(startBlock.neighbors == List((Block(Pos(1,-1),Pos(1,0)),Left), (Block(Pos(1,2),Pos(1,3)),Right), (Block(Pos(-1,1),Pos(0,1)),Up), (Block(Pos(2,1),Pos(3,1)),Down)))

    }
  }

  test("legal neighbors") {
    new Level1 {

      assert(startBlock.legalNeighbors == List((Block(Pos(1,2),Pos(1,3)),Right), (Block(Pos(2,1),Pos(3,1)),Down)))

    }
  }

  test("done"){
    new Level1 {
      val legalBlock = new Block(Pos(1,2),Pos(1,3))
      val illegalBlock =  new Block(Pos(-1,-1),Pos(-1,-1))

      assert(done(startBlock) == false)
      assert(done(legalBlock) == false)
      assert(done(illegalBlock) == false)
      assert(done(endBlock) == true)

    }

  }

  test("neighbors with history"){
    new Level1 {

    assert(neighborsWithHistory(
        Block(Pos(1,1),Pos(1,1)), List()).toSet ==
        Set(
          (Block(Pos(1,2),Pos(1,3)), List(Right)),
          (Block(Pos(2,1),Pos(3,1)), List(Down))
        ))

    assert(neighborsWithHistory(
         Block(Pos(1,1),Pos(1,1)), List(Left,Up)).toSet ==
        Set(
            (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
             (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
            ))

    assert(neighborsWithHistory(
                Block(Pos(1,2),Pos(1,3)), List(Right)).toSet ==
                 Set(
                      (Block(Pos(2,2),Pos(2,3)), List(Down,Right)),
                      (Block(Pos(1,4),Pos(1,4)), List(Right,Right)),
                      (Block(Pos(1,1),Pos(1,1)), List(Left,Right))
                    ))


    }

  }

  test("new neighbors only"){
    new Level1 {

      assert(newNeighborsOnly(
        Set(
          (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
          (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
        ).toStream,
        Set(Block(Pos(1,2),Pos(1,3)), Block(Pos(1,1),Pos(1,1)))) ==
        Set(
          (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
        ).toStream)

    }

  }

  test("optimal solution"){
    new SimpleLevel {

      assert(solution.length == optsolution.length)
      assert(solution == optsolution)

    }

  }

}
