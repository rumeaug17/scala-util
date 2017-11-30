import org.agdf.util._

import scala.util.Random
import scala.collection.immutable._

object state {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  // State monad for a functionnal random generator
  
  
  // the random generator itself
  def NextRandom(state : Int = 0) : (Int, Short) = {
  	val mutliplier = 214013
  	val increment = 2531011
  	val modulus = Int.MaxValue
  	
  	val newState = mutliplier * state + increment ;
  	val rand = ((newState & modulus) >> 16).toShort
  	 (newState, rand)
  }                                               //> NextRandom: (state: Int)(Int, Short)
  

	// using it without state monad
	val seed = 0                              //> seed  : Int = 0
  val a0 = NextRandom(seed)                       //> a0  : (Int, Short) = (2531011,38)
  val a1 = NextRandom(a0._1)                      //> a1  : (Int, Short) = (505908858,7719)
  val a2 = NextRandom(a1._1)                      //> a2  : (Int, Short) = (-755606699,21238)
  
  val result = a0._2 + a1._2 + a2._2              //> result  : Int = 28995
  
  // creating the state generator
  def NextRandomWithState : State[Int, Short] = {
  	State {
  		s =>
  			NextRandom(s)
  	}
  }                                               //> NextRandomWithState: => org.agdf.util.State[Int,Short]
  
  // using it (composition)
  val rs = for {
  	a0 <- NextRandomWithState
  	a1 <- NextRandomWithState
  	a2 <- NextRandomWithState
  } yield (a0 + a1 + a2)                          //> rs  : org.agdf.util.State[Int,Int] = org.agdf.util.State$$anon$1@6b143ee9
  
  // the result
  rs.eval(0)                                      //> res0: Int = 28995
  
  // generate a sequence of random numbers
  def  ListOfRandom(nb : Int, initialSeed : Int = 0) = {
	  val lst = List.fill(nb)(NextRandomWithState)
	  lst.scanLeft((initialSeed, 0 : Short))((seed, r) => r.run(seed._1)).map(_._2).tail
  }                                               //> ListOfRandom: (nb: Int, initialSeed: Int)List[Short]
  
	 ListOfRandom(10)                         //> res1: List[Short] = List(38, 7719, 21238, 2437, 8855, 11797, 8365, 32285, 1
                                                  //| 0450, 30612)
	 ListOfRandom(10, 1000)                   //> res2: List[Short] = List(3304, 8221, 26849, 14038, 1509, 6367, 7856, 21362,
                                                  //|  6968, 10160)

	// using it with the Scala standard random generator
	def NexStdRandom : State[Random, Int] = {
		State {
			r => (r, r.nextInt())
		}
	}                                         //> NexStdRandom: => org.agdf.util.State[scala.util.Random,Int]

	def NexStdPairRandom : State[Random, Int] = {
		val max = Int.MaxValue / 2
		State {
			r => (r,  r.nextInt(max)  * 2)
		}
	}                                         //> NexStdPairRandom: => org.agdf.util.State[scala.util.Random,Int]

  val rs2 = for {
  	a0 <- NexStdRandom
  	a1 <- NexStdRandom
  	a2 <- NexStdRandom
  	b1 <- NexStdPairRandom
  	b2 <- NexStdPairRandom
  } yield (b1 + b2)                               //> rs2  : org.agdf.util.State[scala.util.Random,Int] = org.agdf.util.State$$an
                                                  //| on$1@6574b225
  
  rs2.eval(new Random(0))                         //> res3: Int = 1046952632

  def  ListOfRandom2(nb : Int, initialSeed : Int = 0) = {
	  val lst = List.fill(nb)(NexStdRandom)
	  lst.scanLeft((new Random(initialSeed), Default.value[Int]))((seed, r) => r.run(seed._1)).map(_._2).tail
  }                                               //> ListOfRandom2: (nb: Int, initialSeed: Int)List[Int]
  
  ListOfRandom2(10)                               //> res4: List[Int] = List(-1155484576, -723955400, 1033096058, -1690734402, -1
                                                  //| 557280266, 1327362106, -1930858313, 502539523, -1728529858, -938301587)
  ListOfRandom2(10, 1000)                         //> res5: List[Int] = List(-1244746321, 1060493871, -1826063944, 1976922248, -2
                                                  //| 30127712, 68408698, 169247282, -735843605, 2089114528, 1533708900)
  ListOfRandom2(10).sum                           //> res6: Int = 1727787877
  

  type SRandom[A] = State[Random, A]

  def  ListOfRandom3[A](generator : SRandom[A])(nb : Int)(initialSeed : Int) = {
	  val lst = List.fill(nb)(generator)
	  lst.scanLeft((new Random(initialSeed), Default.value[A]))((seed, r) => r.run(seed._1)).map(_._2).tail
  }                                               //> ListOfRandom3: [A](generator: state.SRandom[A])(nb: Int)(initialSeed: Int)L
                                                  //| ist[A]

	val BoolRandomList = ListOfRandom3(State { r => (r, r.nextBoolean()) })_
                                                  //> BoolRandomList  : Int => (Int => List[Boolean]) = <function1>
	BoolRandomList(10)(0)                     //> res7: List[Boolean] = List(true, true, false, true, true, false, true, fals
                                                  //| e, true, true)

	val EvenRandomList = ListOfRandom3(NexStdPairRandom)_
                                                  //> EvenRandomList  : Int => (Int => List[Int]) = <function1>
	EvenRandomList(15)(2)                     //> res8: List[Int] = List(992768570, 1260042744, 1724204434, 17850134, 2133836
                                                  //| 778, 1523343054, 2086825566, 142959438, 1533836048, 1817306490, 2093456542,
                                                  //|  1945002172, 979930868, 85689582, 321237028)
			
}