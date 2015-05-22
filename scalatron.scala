// Tutorial Bot #10: Food Finder
// Step 5: exploiting the nearest food item

class ControlFunction() {
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
        
        "Move(direction=" + view.actualPerformance().toString + ")"        
    
    
    }
    
    def reactAsSlave(view: View, params: Map[String, String]) = "Status(text=Slave)"
}

case class View(cells: String) {
    
    val GOOD_BOT = ('B',400)
    val BAD_BOT = ('b',-200)
    val GOOD_PLANT = ('P',300)
    val BAD_PLANT = ('p',-100)
    val UNKNOWN = ('?',-40)
    val WALL = ('W',-100)
    val EMPTY = ('_',0)
    val MASTER_BOT = 'M'
    val MINI_BOT = 'S'
    val BASE = 1.5
    
    val size = math.sqrt(cells.length).toInt
    val center = XY(size/2, size/2)

    def offsetToNearest(c: Char) = {
        val relativePositions =
            cells
            .view
            .zipWithIndex
            .filter(_._1 == c)
            .map(p => relPosFromIndex(p._2))
        if(relativePositions.isEmpty)
            None
        else
            Some(relativePositions.minBy(_.length))
    }
    
    def cellValue(c:Char):Double = {
        c match {
            case GOOD_BOT._1 => GOOD_BOT._2.toDouble
            case BAD_BOT._1 => BAD_BOT._2.toDouble
            case GOOD_PLANT._1 => GOOD_PLANT._2.toDouble
            case BAD_PLANT._1 => BAD_PLANT._2.toDouble
            case UNKNOWN._1 => UNKNOWN._2.toDouble
            case WALL._1 => WALL._2.toDouble
            case EMPTY._1 => EMPTY._2.toDouble
            case _ => 0.0
        }
    }
    
    def calculatePerformance(cells_array: Seq[(Char,Int)] ,cells_position: XY):Double = {
        
        val performance =
            performancePerDistance(cells_array, GOOD_BOT._1, cells_position) + 
            performancePerDistance(cells_array, BAD_BOT._1, cells_position) +
            performancePerDistance(cells_array, GOOD_PLANT._1, cells_position) +
            performancePerDistance(cells_array, BAD_PLANT._1, cells_position) +
            performancePerDistance(cells_array, UNKNOWN._1, cells_position)   +
            cellValue(cells_array(indexFromAbsPos(cells_position))._1)
        return performance
    }
    
    
    def actualPerformance():XY = {
        
        val cells_array = cells.view.zipWithIndex   

        val destination = Array(XY.Right,XY.RightUp,XY.Up,XY.UpLeft,XY.Left,XY.LeftDown,XY.Down,XY.DownRight)
        
        val performance = destination.map(p => calculatePerformance(cells_array, absPosFromRelPos(p))).zipWithIndex.max
        
        return destination(performance._2)
    }
    
    
    def performancePerDistance(arrayCharsIndeces: Seq[(Char,Int)], entity: Char, destination: XY):Double = {

        val entityStepsFromDestination = arrayCharsIndeces
        .filter(_._1 == entity)           .filter(_ != destination)
        .map(p => absPosFromIndex(p._2)) 
        .map(calculateRequiredSteps(_,destination))
        .filter( _ <= 10)
        
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

