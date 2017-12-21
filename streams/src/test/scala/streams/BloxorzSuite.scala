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

	test("Position equality check") {
    new Level1 {
      assert(Pos(0,0) == Pos(0,0), "Equality check")
    }
	}
	
	test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(terrain(Pos(1,1)), "1,1") // start
      assert(terrain(Pos(4,7)), "4,7") // goal
      assert(terrain(Pos(5,8)), "5,8")
      assert(!terrain(Pos(5,9)), "5,9")
      assert(terrain(Pos(4,9)), "4,9")
      assert(!terrain(Pos(6,8)), "6,8")
      assert(!terrain(Pos(4,11)), "4,11")
      assert(!terrain(Pos(-1,0)), "-1,0")
      assert(!terrain(Pos(0,-1)), "0,-1")
    }
  }

	test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1,1))
      assert(goal == Pos(4,7))
    }
  }

	test("Block Tests") {
    new Level1 {
      assert(startBlock.isLegal, "Is start block legal")
      assert(startBlock.b1 == Pos(1, 1), "Start block b1 position")
      assert(startBlock.b2 == Pos(1, 1), "Start block b2 position")
      assert(startBlock.isStanding, "Is start block in standing position")
    }
	}

  test("Block Direction Tests") {
    new Level1 {
      assert(startBlock.left.b1 == Pos(1, -1), "Start block left b1 position")
      assert(startBlock.left.b2 == Pos(1, 0), "Start block left b2 position")
      assert(startBlock.right.b1 == Pos(1, 2), "Start block right b1 position")
      assert(startBlock.right.b2 == Pos(1, 3), "Start block right b2 position")
      assert(startBlock.up.b1 == Pos(-1, 1), "Start block up b1 position")
      assert(startBlock.up.b2 == Pos(0, 1), "Start block up b2 position")
      assert(startBlock.down.b1 == Pos(2, 1), "Start block down b1 position")
      assert(startBlock.down.b2 == Pos(3, 1), "Start block down b2 position")
    }
	}

  test("Block Neighbors Tests") {
    new Level1 {
      assert(startBlock.neighbors(0)._1 == startBlock.left, "Left of start block is a neighbor")
      assert(startBlock.neighbors(1)._1 == startBlock.right, "Right of start block is a neighbor")
      assert(startBlock.neighbors(2)._1 == startBlock.up, "Up of start block is a neighbor")
      assert(startBlock.neighbors(3)._1 == startBlock.down, "Down of start block is a neighbor")
    }
	}

  test("Block Neighbors Move Tests") {
    new Level1 {
      assert(!startBlock.left.isLegal, "Left of start block is not a legal move")
      assert(startBlock.right.isLegal, "Right of start block is a legal move")
      assert(!startBlock.up.isLegal, "Up of start block is not a legal move")
      assert(startBlock.down.isLegal, "Down of start block is a legal move")
    }
	}

  test("Block Legal Neighbors Tests") {
    new Level1 {
      assert(startBlock.legalNeighbors.size == 2, "Start block has two legal neighbors")
      assert(startBlock.legalNeighbors(0)._1 != startBlock.left, "Left of start block is not a legal neighbor")
      assert(startBlock.legalNeighbors(0)._1 == startBlock.right, "Right of start block is a legal neighbor")
      assert(startBlock.legalNeighbors(1)._1 != startBlock.up, "Up of start block is not a legal neighbor")
      assert(startBlock.legalNeighbors(1)._1 == startBlock.down, "Down of start block is a legal neighbor")
    }
	}

	test("Solver done Tests") {
    new Level1 {
      assert(done(Block(goal, goal)))
      assert(!done(startBlock))
    }
  }
  
  test("Legal Neighbors With History Tests") {
    new Level1 {
      val s = neighborsWithHistory(startBlock, List())
      assert(s.size == 2)
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

}
