package com.acrylplatform.api.http

import akka.actor.ActorSystem
import akka.http.scaladsl.model.HttpMethods._
import akka.http.scaladsl.model.headers._
import akka.http.scaladsl.model.{HttpRequest, HttpResponse, StatusCodes}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.RouteResult.Complete
import akka.http.scaladsl.server.directives.{DebuggingDirectives, LoggingMagnet}
import akka.http.scaladsl.server.{Route, RouteResult}
import akka.stream.ActorMaterializer
import com.acrylplatform.api.http.swagger.SwaggerDocService
import com.acrylplatform.settings.RestAPISettings
import com.acrylplatform.utils.ScorexLogging

import scala.concurrent.Future

case class CompositeHttpService(apiTypes: Set[Class[_]], routes: Seq[ApiRoute], settings: RestAPISettings)(implicit system: ActorSystem)
    extends ScorexLogging {

  private implicit val materializer: ActorMaterializer = ActorMaterializer()

  private val swaggerService    = new SwaggerDocService(system, ActorMaterializer()(system), apiTypes, settings)
  private val redirectToSwagger = redirect("/api-docs/index.html", StatusCodes.PermanentRedirect)
  private val swaggerRoute: Route = swaggerService.routes ~
    (pathEndOrSingleSlash | path("swagger"))(redirectToSwagger) ~
    pathPrefix("api-docs") {
      pathEndOrSingleSlash(redirectToSwagger) ~ getFromResourceDirectory("swagger-ui")
    }

  val compositeRoute: Route                             = extendRoute(routes.map(_.route).reduce(_ ~ _)) ~ swaggerRoute ~ complete(StatusCodes.NotFound)
  val loggingCompositeRoute: Route                      = DebuggingDirectives.logRequestResult(LoggingMagnet(_ => logRequestResponse))(compositeRoute)
  val asyncHandler: HttpRequest => Future[HttpResponse] = Route.asyncHandler(loggingCompositeRoute)

  private def logRequestResponse(req: HttpRequest)(res: RouteResult): Unit = res match {
    case Complete(resp) =>
      val msg = s"HTTP ${resp.status.value} from ${req.method.value} ${req.uri}"
      if (resp.status == StatusCodes.OK) log.info(msg) else log.warn(msg)
    case _ =>
  }

  private val corsAllowedHeaders = (if (settings.apiKeyDifferentHost) List("api_key", "X-API-Key") else List.empty[String]) ++
    Seq("Authorization", "Content-Type", "X-Requested-With", "Timestamp", "Signature")

  private def corsAllowAll = if (settings.cors) respondWithHeader(`Access-Control-Allow-Origin`.*) else pass

  private def extendRoute(base: Route): Route =
    if (settings.cors) { ctx =>
      val extendedRoute = corsAllowAll(base) ~ options {
        respondWithDefaultHeaders(
          `Access-Control-Allow-Credentials`(true),
          `Access-Control-Allow-Headers`(corsAllowedHeaders),
          `Access-Control-Allow-Methods`(OPTIONS, POST, PUT, GET, DELETE)
        )(corsAllowAll(complete(StatusCodes.OK)))
      }

      extendedRoute(ctx)
    } else base
}
