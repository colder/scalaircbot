package ircbot.modules.helpers

trait Http {
    class HttpException  extends RuntimeException

    import org.apache.commons.httpclient._, methods._, params._, cookie._

    def httpRequest(url: String): java.io.InputStream  = httpRequest(url, 1);

    def httpRequest(url: String, timeout: Int): java.io.InputStream = {
        val client = new HttpClient()
        val method = new GetMethod(url)

        client.getHttpConnectionManager.getParams.setConnectionTimeout(timeout*1000)

        method.getParams().setParameter(HttpMethodParams.RETRY_HANDLER, new DefaultHttpMethodRetryHandler(3, false))

        client.executeMethod(method)

        method.getResponseBodyAsStream()
    }

}
