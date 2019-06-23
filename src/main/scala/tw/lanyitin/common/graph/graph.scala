package tw.lanyitin.common.graph

case class Node[V] (val payload: V)

sealed trait Edge[V, U] {
  def extractData: (Node[V], Node[V], U)
}
case class UndirectedEdge[V, U](val node1: Node[V], val node2: Node[V], val annotation: U = null) extends Edge[V, U] {
  def extractData = (node1, node2, annotation)
}
case class DirectedEdge[V, U](val from: Node[V], val to: Node[V], val annotation: U = null) extends Edge[V, U] {
  def extractData = (from, to, annotation)
}

case class Path[V, U](edges: List[DirectedEdge[V, U]]) {
  override def toString: String = {
    val head = this.edges.head
    (head.from + " -" + head.annotation + "-> " + head.to :: this.edges.tail.map(edge => " -" + edge.annotation + "-> " + edge.to))
    .mkString
  }
}

case class Graph[V, U] (val nodes: Set[Node[V]], val edges: Set[Edge[V, U]]) {
  def adjacentNodes(node: Node[V]): Set[Node[V]] = {
    this.edges
      .flatMap(edge => edge match {
        case UndirectedEdge(node1, node2, payload) => {
          if (node1 == node) {
            List(node2)
          } else if (node2 == node) {
            List(node1)
          } else {
            List()
          }
        }
        case DirectedEdge(from, to, payload) => if (from == node) List(to) else List()
      })
      .toSet
  }

  def childNodes(node: Node[V]): Set[Node[V]] = {
    this.directedEdges
      .filter(diEdge => diEdge.from == node)
      .map(diEdge => diEdge.to)
  }

  def parentNodes(node: Node[V]): Set[Node[V]] = {
    this.directedEdges
      .filter(diEdge => diEdge.to == node)
      .map(diEdge => diEdge.from)
  }

  def outgoingEdges(node: Node[V]): Set[DirectedEdge[V, U]] = {
    val edgesFromDirected = this.directedEdges
      .filter(diEdge => diEdge.from == node)
      .toSet

    val edgesFromUndirected = this.undirectedEdges
      .flatMap(edge => {
        if (edge.node1 == node) {
          List(DirectedEdge(edge.node1, edge.node2, edge.annotation))
        } else if (edge.node2 == node) {
          List(DirectedEdge(edge.node2, edge.node1, edge.annotation))
        } else {
          List()
        }
      }).toSet
    edgesFromDirected.union(edgesFromUndirected)
  }

  def incomingEdges(node: Node[V]): Set[DirectedEdge[V, U]] = {
    val edgesFromDirected = this.directedEdges
      .filter(diEdge => diEdge.to == node)
      .toSet

    val edgesFromUndirected = this.undirectedEdges
      .flatMap(edge => {
        if (edge.node1 == node) {
          List(DirectedEdge(edge.node1, edge.node2, edge.annotation))
        } else if (edge.node2 == node) {
          List(DirectedEdge(edge.node2, edge.node1, edge.annotation))
        } else {
          List()
        }
      }).toSet
    edgesFromDirected.union(edgesFromUndirected)
  }

  def undirectedEdges: Set[UndirectedEdge[V, U]] = {
    this.edges
      .filter(_.isInstanceOf[UndirectedEdge[V, U]])
      .map(_.asInstanceOf[UndirectedEdge[V, U]])
  }

  def directedEdges: Set[DirectedEdge[V, U]] = {
    this.edges
      .filter(_.isInstanceOf[DirectedEdge[V, U]])
      .map(_.asInstanceOf[DirectedEdge[V, U]])
  }

  def beginNodes: Set[Node[V]] = {
    this.nodes -- (this.directedEdges.map(edge => edge.to))
  }

  def endNodes: Set[Node[V]] = {
    this.nodes -- (this.directedEdges.map(edge => edge.from))
  }

  def isCompletePath(path: Path[V, U]): Boolean = {
    if (path.edges.length == 0) {
      false
    } else {
      val beginSet = this.beginNodes
      val endSet = this.endNodes
      beginSet.contains(path.edges.head.from) && endSet.contains(path.edges.last.to)
    }
  }

  def addNode(node: Node[V]): Graph[V, U] = {
    Graph(nodes + node, edges)
  }

  def addNodes(_nodes: Array[Node[V]]): Graph[V, U] = {
    Graph(nodes ++ _nodes, edges)
  }

  def removeNode(node: Node[V]): Graph[V, U] = {
    Graph(nodes - node, edges.filter(e => {
      e match {
        case DirectedEdge(n1, n2, _) => n1 != node && n2 != node
        case UndirectedEdge(n1, n2, _) => n1 != node && n2 != node
      }
    }))
  }

  def addEdge(edge: Edge[V, U]): Graph[V, U] = {
    val (n1, n2, _) = edge.extractData
    Graph(nodes ++ Set(n1, n2), edges + edge)
  }
}

trait GraphFactory[V, U] {
  def pseudoNode: Node[V]
  def pseudoEdge(node1: Node[V], node2: Node[V]): Edge[V, U]
  def duplicateNode(node: Node[V]): Node[V]
}