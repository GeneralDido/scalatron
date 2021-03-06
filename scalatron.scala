// Tutorial Bot #10: Food Finder
// Step 5: exploiting the nearest food item
import util.Random

class ControlFunction() {
    
    val rnd = new Random
    // this method is called by the server
    def respond(input: String): String = {
        val (opcode, params) = CommandParser(input)
        opcode match {
            case "Welcome" =>
                welcome(
                    params("name"),
                    params("path"),
                    params("apocalypse").toInt,
                    params("round").toInt
                )
            case "React" =>
                react(
                    params("generation").toInt,
                    View(params("view")),
                    params
                )
            case "Goodbye" =>
                goodbye(
                    params("energy").toInt
                )
            case _ =>
                "" // OK
        }
    }

    def welcome(name: String, path: String, apocalypse: Int, round: Int) = ""

    def react(generation: Int, view: View, params: Map[String, String]) = 
        if( generation == 0 ) reactAsMaster(view, params)
        else reactAsSlave(view, params)
    
    def goodbye(energy: Int) = ""

    def reactAsMaster(view: View, params: Map[String, String]) = {
        val direction = view.actualPerformance(false)._1.toString
        if (rnd.nextDouble() < 0.6)
            "Move(direction=" + direction + ")|Spawn(direction=" + direction + ",energy=100,heading=" + direction + ")"
        else
            "Move(direction=" + direction + ")"
    }
    def reactAsSlave(view: View, params: Map[String, String]) = {
        
        val output = view.actualPerformance(true)
        val direction = output._1.toString
        val EnemyIsNear = output._2
        if (EnemyIsNear == 1000000)
            "Explode(size=3)"
        else if (params(("energy")).toInt >=4000)
            "Move(direction=" + params("master") + ")"
       else if (params("generation").toInt < 3 && rnd.nextDouble() < 0.2)
            "Move(direction=" + direction + ")|Spawn(direction=" + direction + ",energy=100,heading=" + direction + ")"
        else
            "Move(direction=" + direction + ")"
    }
}

case class View(cells: String) {
    
    val GOOD_BOT = ('B',400)
    val BAD_BOT = ('b',-200)
    val GOOD_PLANT = ('P',300)
    val BAD_PLANT = ('p',-100)
    val UNKNOWN = ('?',-35)
    val WALL = ('W',-100)
    val EMPTY = ('_',0)
    val ENEMY_MASTER = ('m', -200)
    val ENEMY_MINI = ('s', -200)
    val MASTER_BOT = 'M'
    val MINI_BOT = ('S',500)
    val BASE = 1.5
    
    val size = math.sqrt(cells.length).toInt
    val center = XY(size/2, size/2)
    
    def cellValue(c:Char):Double = {
        c match {
            case GOOD_BOT._1 => GOOD_BOT._2.toDouble
            case BAD_BOT._1 => BAD_BOT._2.toDouble
            case GOOD_PLANT._1 => GOOD_PLANT._2.toDouble
            case BAD_PLANT._1 => BAD_PLANT._2.toDouble
            case UNKNOWN._1 => UNKNOWN._2.toDouble
            case WALL._1 => WALL._2.toDouble
            case EMPTY._1 => EMPTY._2.toDouble
            case MINI_BOT => MINI_BOT._2.toDouble
            case ENEMY_MASTER => ENEMY_MASTER._2.toDouble
            case ENEMY_MINI => ENEMY_MINI._2.toDouble
            case _ => 0.0
        }
    }
    
    def calculatePerformance(cells_array: Seq[(Char,Int)] ,cells_position: XY, isMini: Boolean):Double = {
                
        val entities = Array(GOOD_BOT._1, GOOD_PLANT._1, BAD_BOT._1, BAD_PLANT._1, UNKNOWN._1)
        val enemy = Array(ENEMY_MASTER._1, ENEMY_MINI._1)        
        val miniBots = performancePerDistance(cells_array, MINI_BOT._1, cells_position)
        val cell = cells_array(indexFromAbsPos(cells_position))._1
        val cellvalue = cellValue(cell)

        val DetonateEnemy = enemy.map(performancePerDistance(cells_array, _, cells_position)).foldLeft[Double](0)(_ + _)
        val path = entities.map(performancePerDistance(cells_array, _, cells_position)).foldLeft[Double](0)(_ + _)
        
        var performance = path + DetonateEnemy + cellvalue

        if (isMini)
            if(DetonateEnemy <= -100.00)
                return 1000000
            else if (cell == MINI_BOT)
                return performance - miniBots - 2 * cellvalue
            else
                return performance - miniBots
        else
            return performance + miniBots
    } 
    
    
    def actualPerformance(isMini: Boolean):(XY,Double) = {
        
        val cells_array = cells.view.zipWithIndex   

        val destination = Array(XY.Right,XY.RightUp,XY.Up,XY.UpLeft,XY.Left,XY.LeftDown,XY.Down,XY.DownRight)
        
        val performance = destination.map(p => calculatePerformance(cells_array, absPosFromRelPos(p), isMini)).zipWithIndex.max
        
        return (destination(performance._2),performance._1)
    }
    
    
    def performancePerDistance(arrayCharsIndeces: Seq[(Char,Int)], entity: Char, destination: XY):Double = {

        val entityStepsFromDestination = arrayCharsIndeces
        .filter(_._1 == entity).filter(_ != destination)
        .map(p => absPosFromIndex(p._2))
        .map(calculateRequiredSteps(_,destination))
        
        entityStepsFromDestination.map(p => math.pow(BASE,-p)).map(cellValue(entity)*_).foldLeft[Double](0)(_ + _)
        
        
    }

    def apply(relPos: XY) = cellAtRelPos(relPos)

    def indexFromAbsPos(absPos: XY) = absPos.x + absPos.y * size
    def absPosFromIndex(index: Int) = XY(index % size, index / size)
    def absPosFromRelPos(relPos: XY) = relPos + center
    def cellAtAbsPos(absPos: XY) = cells.apply(indexFromAbsPos(absPos))

    def indexFromRelPos(relPos: XY) = indexFromAbsPos(absPosFromRelPos(relPos))
    def relPosFromAbsPos(absPos: XY) = absPos - center
    def relPosFromIndex(index: Int) = relPosFromAbsPos(absPosFromIndex(index))
    def cellAtRelPos(relPos: XY) = cells(indexFromRelPos(relPos))
    
    def calculateRequiredSteps(source: XY, destination: XY): Int = math.max(math.abs(source.x - destination.x), 
    math.abs(source.y - destination.y))

}

case class XY(x: Int, y: Int) {
    override def toString = x + ":" + y

    def isNonZero = x!=0 || y!=0
    def isZero = x==0 && y==0
    def isNonNegative = x>=0 && y>=0

    def updateX(newX: Int) = XY(newX, y)
    def updateY(newY: Int) = XY(x, newY)

    def addToX(dx: Int) = XY(x+dx, y)
    def addToY(dy: Int) = XY(x, y+dy)

    def +(pos: XY) = XY(x+pos.x, y+pos.y)
    def -(pos: XY) = XY(x-pos.x, y-pos.y)
    def *(factor: Double) = XY((x*factor).intValue, (y*factor).intValue)

    def distanceTo(pos: XY) : Double = (this-pos).length
    def length : Double = math.sqrt(x*x + y*y)

    def signum = XY(x.signum, y.signum)

    def negate = XY(-x, -y)
    def negateX = XY(-x, y)
    def negateY = XY(x, -y)

}
object XY {
    def apply(s: String) : XY = {
        val xy = s.split(':').map(_.toInt) // e.g. "-1:1" => Array(-1,1)
        XY(xy(0), xy(1))
    }

    val Zero = XY(0,0)
    val One =  XY(1,1)

    val Right      = XY( 1,  0)
    val RightUp    = XY( 1, -1)
    val Up         = XY( 0, -1)
    val UpLeft     = XY(-1, -1)
    val Left       = XY(-1,  0)
    val LeftDown   = XY(-1,  1)
    val Down       = XY( 0,  1)
    val DownRight  = XY( 1,  1)
}

object CommandParser {
    def apply(command: String) = {
        def splitParam(param: String) = {
            val segments = param.split('=')
            if( segments.length != 2 )
                throw new IllegalStateException("invalid key/value pair: " + param)
            (segments(0),segments(1))
        }

        val segments = command.split('(')
        if( segments.length != 2 )
            throw new IllegalStateException("invalid command: " + command)

        val params = segments(1).dropRight(1).split(',')
        val keyValuePairs = params.map( splitParam ).toMap
        (segments(0), keyValuePairs)
    }
}

class ControlFunctionFactory {
    def create = new ControlFunction().respond _
}

