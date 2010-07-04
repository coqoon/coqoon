import scala.actors._
import scala.actors.Actor._

object SillyActor extends Actor
{
	def act() {
		for (i <- 1 to 5) {
			println("I'm acting!")
			Thread.sleep(1000)
		}
	}
}

object SeriousActor extends Actor 
{ 
	def act() { 
		for (i <- 1 to 5) { 
			println("To be or not to be.") 
			Thread.sleep(1000) 
		} 
	} 
} 

object Misc
{
	val echoActor = actor { 
		while (true) { 
			receive { 
				case msg => 
					println("received message: " + msg) 
			} 
		} 
	}
}

object NameResolver extends Actor 
{ 
	import java.net.{InetAddress, UnknownHostException} 
	
	def act() = { 
		receive { 
			case (name: String, actor: Actor) => 
				actor ! getip(name) 
				act() 
			case "EXIT" => 
				println("Name resolver exiting.") 
			// quit 
			case msg => 
				println("Unhandled message: " + msg) 
				act() 
		} 
	} 
	
	def getip(name: String): Option[InetAddress] = { 
		try { 
			Some(InetAddress.getByName(name)) 
		} catch { 
			case _:UnknownHostException => None 
		} 
	}
} 
