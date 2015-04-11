class Seizer {

	import scala.io._
	import scala.actors._
	import Actor._

	val linkRegex = "(?i)<a.+?href=\"(http.+?)\".*?>(.+?)</a>".r

	object PageLoader {

		def load(url: String) = {
			try {
				Source.fromURL(url).mkString
			} catch {
			case e: Exception => System.err.println(e)
					""
			}
		}

		def getPageSize(url: String) = load(url).length;

		def getPageSizeAndLinks(url: String) = {
			val content = load(url);
			//this gets the second group of the regex (http...)
			val links = linkRegex.findAllIn(content).matchData.toList.map(_.group(1)) 
            println("Found " + links.length + " links");  
			(content.length, links)
		}
	}

	val urls = List("http://admios.com");

	execute();


	def timeMethod(method: () => Unit) = {
		val start = System.nanoTime;
		method()
		val end = System.nanoTime;
		println("Method took " + (end - start)/1000000000.0 + " seconds.")
	}

	def getPageSizeSequentially() = {
		for(url <- urls) {

			val (size, links) = PageLoader.getPageSizeAndLinks(url);
			var otherLinkSizes = size;
			for(link <- links){
				otherLinkSizes = otherLinkSizes + PageLoader.getPageSize(link)
			}
			println(url, size, otherLinkSizes);
		}
	}

	def getPageSizeConcurrently() = {
		val caller = self;


		for(url <- urls){

			actor{
				val (size, links) = PageLoader.getPageSizeAndLinks(url); 

				val linkActor = self;

				for(link <-links){
					actor { linkActor ! PageLoader.getPageSize(link) }
				}
				var totalSize = size; 

				for (i <- 1 to links.length) {
					receive { case linkSize:Int => totalSize = totalSize + linkSize
					}
				}
				caller ! (url,  size, links, totalSize);
			}
		}

		for (i <- 1 to urls.length) {    
			receive {
			     case (url,size ,links, totalSize: Int) => println(url, size, totalSize);
			}
		}
	}

	def execute() = {
		println("Sequential run:")
		timeMethod { getPageSizeSequentially }

		println("Concurrent run")
		timeMethod { getPageSizeConcurrently }

	}
}



val seizer = new Seizer();