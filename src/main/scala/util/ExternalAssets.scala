package util

import java.nio.file.{Files, Path, Paths, StandardCopyOption}

/** Locates and caches external test assets (see README "External test assets").
  *
  * If `$CPU_LEGO_ASSETS/<name>` exists it is used as-is; otherwise the file is fetched once from `url` and cached
  * there. When the asset cannot be obtained the lookup returns [[Left]] with the cause, and tests that need it skip
  * with a clear message instead of failing.
  */
object ExternalAssets {

  /** Directory holding cached assets: `$CPU_LEGO_ASSETS` when set, otherwise `~/.cache/cpu-lego`.
    */
  def cacheDir: Path =
    sys.env
      .get("CPU_LEGO_ASSETS")
      .map(Paths.get(_))
      .getOrElse(Paths.get(sys.props("user.home"), ".cache", "cpu-lego"))

  /** Returns the local path of the cached asset `name`, fetching it from `url` and caching it on first use. Returns
    * [[Left]] with the cause when the asset is not cached and cannot be fetched.
    */
  def fetch(name: String, url: String): Either[String, Path] = {
    val target = cacheDir.resolve(name)
    if (Files.isRegularFile(target)) {
      Right(target)
    } else {
      download(url, target)
    }
  }

  private def download(url: String, target: Path): Either[String, Path] = {
    try {
      Files.createDirectories(target.getParent)
      val tmp = Files.createTempFile(target.getParent, target.getFileName.toString, ".tmp")
      try {
        val in = new java.net.URI(url).toURL.openStream()
        try {
          Files.copy(in, tmp, StandardCopyOption.REPLACE_EXISTING)
        } finally {
          in.close()
        }
        Files.move(tmp, target, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE)
        Right(target)
      } finally {
        Files.deleteIfExists(tmp)
      }
    } catch {
      case e: Exception => Left(s"$url: ${e.getMessage}")
    }
  }
}
