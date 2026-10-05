package util

import java.nio.file.{Files, Path, Paths, StandardCopyOption}

/** Locates and caches freely-licensed external test assets.
  *
  * Some tests need data files that are too large (or too oddly licensed) to vendor in the repo — e.g. the
  * SingleStepTests 6502 suites. This helper is the single place that knows where those files live:
  *
  *   - if `$CPU_LEGO_ASSETS/<name>` already exists it is used as-is, so a user-supplied file (dropped in by hand)
  *     always wins;
  *   - otherwise the file is fetched once from `url` and cached there.
  *
  * When the asset cannot be obtained (no network, fetch failed) the lookup returns [[None]] and tests that need it skip
  * with a clear message instead of failing. In restricted environments, pre-populate `$CPU_LEGO_ASSETS` (or
  * `~/.cache/cpu-lego`) by hand — e.g. with curl — and the tests will pick the files up without any download.
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
    * [[None]] when the asset is not cached and cannot be fetched.
    */
  def fetch(name: String, url: String): Option[Path] = {
    val target = cacheDir.resolve(name)
    if (Files.isRegularFile(target)) {
      Some(target)
    } else {
      download(url, target)
    }
  }

  private def download(url: String, target: Path): Option[Path] = {
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
        Some(target)
      } finally {
        Files.deleteIfExists(tmp)
      }
    } catch {
      case _: Exception => None
    }
  }
}
