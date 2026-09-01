(function () {
  "use strict";

  var SITE_ROOT = (function () {
    var self = document.currentScript;
    if (!self || !self.src) return "/";
    return self.src.replace(/assets\/js\/booklet-flipbook\.js(\?.*)?$/, "");
  })();

  function asset(path) {
    return SITE_ROOT + String(path).replace(/^\//, "");
  }

  var PDFJS_SRC = asset("assets/vendor/pdfjs/pdf.min.mjs");
  var PDFJS_WORKER_SRC = asset("assets/vendor/pdfjs/pdf.worker.min.mjs");
  var PAGEFLIP_SRC = asset("assets/vendor/page-flip/page-flip.browser.js");

  var RENDER_AHEAD = 3;
  var KEEP_AHEAD = 5;

  var MAX_CANVAS_WIDTH = 1600;

  // Two pages side by side only make sense when each of them stays readable.
  var SPREAD_MIN_STAGE_WIDTH = 760;
  var SPREAD_MIN_PAGE_WIDTH = 320;

  // Ignore the small viewport changes mobile browsers emit while their
  // toolbars slide in and out; only a real size change rebuilds the book.
  var RELAYOUT_THRESHOLD = 32;
  var RESIZE_DEBOUNCE = 220;

  var prefersReducedMotion = window.matchMedia
    ? window.matchMedia("(prefers-reduced-motion: reduce)").matches
    : false;

  var pdfjsPromise = null;
  var pageFlipPromise = null;

  function loadPdfJs() {
    if (!pdfjsPromise) {
      pdfjsPromise = import(PDFJS_SRC).then(function (lib) {
        lib.GlobalWorkerOptions.workerSrc = PDFJS_WORKER_SRC;
        return lib;
      });
    }
    return pdfjsPromise;
  }

  function loadPageFlip() {
    if (!pageFlipPromise) {
      pageFlipPromise = new Promise(function (resolve, reject) {
        if (window.St && window.St.PageFlip) {
          resolve(window.St.PageFlip);
          return;
        }
        var script = document.createElement("script");
        script.src = PAGEFLIP_SRC;
        script.onload = function () {
          if (window.St && window.St.PageFlip) {
            resolve(window.St.PageFlip);
          } else {
            reject(new Error("page-flip loaded but exposed no PageFlip"));
          }
        };
        script.onerror = function () {
          reject(new Error("could not load page-flip"));
        };
        document.head.appendChild(script);
      });
    }
    return pageFlipPromise;
  }

  function el(tag, className, text) {
    var node = document.createElement(tag);
    if (className) node.className = className;
    if (text) node.textContent = text;
    return node;
  }

  function Reader(config) {
    this.config = config;
    this.pages = [];
    this.doc = null;
    this.flip = null;
    this.dialog = null;
    this.destroyed = false;
    this.spread = false;
    this.pageWidth = 0;
    this.pageHeight = 0;
  }

  Reader.prototype.open = function () {
    var self = this;

    this.buildDialog();
    document.body.appendChild(this.dialog);
    this.lockPageScroll();
    this.dialog.showModal();
    this.bindResize();

    this.loadDocument()
      .then(function (doc) {
        self.doc = doc;
        return self.buildPages();
      })
      .then(function () {
        return self.initFlip();
      })
      .then(function () {
        self.setStatus(null);
      })
      .catch(function (error) {
        if (!self.destroyed) self.showError(error);
      });
  };

  Reader.prototype.buildDialog = function () {
    var self = this;

    var dialog = el("dialog", "booklet-reader");
    dialog.setAttribute("aria-label", this.config.title || "Booklet");

    var frame = el("div", "booklet-reader__frame");

    var bar = el("div", "booklet-reader__bar");

    var title = el("p", "booklet-reader__title", this.config.title || "Booklet");

    var actions = el("div", "booklet-reader__actions");

    var download = el("a", "booklet-reader__download", "PDF letöltése");
    download.href = this.config.original || this.config.pdf;
    download.setAttribute("download", "");

    actions.appendChild(download);

    if (document.fullscreenEnabled && frame.requestFullscreen) {
      var fullscreen = el("button", "booklet-reader__fullscreen");
      fullscreen.type = "button";
      fullscreen.setAttribute("aria-label", "Teljes képernyő");
      fullscreen.innerHTML = "<span aria-hidden='true'>&#9974;</span>";
      fullscreen.addEventListener("click", function () {
        self.toggleFullscreen();
      });
      actions.appendChild(fullscreen);
      this.fsBtn = fullscreen;

      this.onFullscreenChange = this.handleFullscreenChange.bind(this);
      document.addEventListener("fullscreenchange", this.onFullscreenChange);
    }

    var close = el("button", "booklet-reader__close");
    close.type = "button";
    close.setAttribute("aria-label", "Bezárás");
    close.innerHTML = "<span aria-hidden='true'>&times;</span>";
    close.addEventListener("click", function () {
      self.close();
    });

    actions.appendChild(close);
    bar.appendChild(title);
    bar.appendChild(actions);

    var stage = el("div", "booklet-reader__stage");
    var book = el("div", "booklet-reader__book");
    stage.appendChild(book);

    var status = el("p", "booklet-reader__status", "Booklet betöltése…");
    stage.appendChild(status);

    var nav = el("div", "booklet-reader__nav");

    var prev = el("button", "booklet-reader__page-btn", "← Előző");
    prev.type = "button";
    prev.addEventListener("click", function () {
      if (self.flip) self.flip.flipPrev();
    });

    var counter = el("span", "booklet-reader__counter", "");

    var next = el("button", "booklet-reader__page-btn", "Következő →");
    next.type = "button";
    next.addEventListener("click", function () {
      if (self.flip) self.flip.flipNext();
    });

    nav.appendChild(prev);
    nav.appendChild(counter);
    nav.appendChild(next);

    frame.appendChild(bar);
    frame.appendChild(stage);
    frame.appendChild(nav);
    dialog.appendChild(frame);

    dialog.addEventListener("cancel", function (event) {
      event.preventDefault();
      self.close();
    });
    dialog.addEventListener("click", function (event) {
      if (event.target === dialog) self.close();
    });

    dialog.addEventListener("keydown", function (event) {
      if (!self.flip) return;
      if (event.key === "ArrowLeft") {
        self.flip.flipPrev();
      } else if (event.key === "ArrowRight") {
        self.flip.flipNext();
      }
    });

    this.dialog = dialog;
    this.frame = frame;
    this.stageEl = stage;
    this.bookEl = book;
    this.statusEl = status;
    this.counterEl = counter;
  };

  Reader.prototype.lockPageScroll = function () {
    this.previousOverflow = document.documentElement.style.overflow;
    document.documentElement.style.overflow = "hidden";
  };

  Reader.prototype.unlockPageScroll = function () {
    document.documentElement.style.overflow = this.previousOverflow || "";
  };

  Reader.prototype.bindResize = function () {
    var self = this;

    this.onViewportChange = function () {
      if (self.destroyed) return;
      window.clearTimeout(self.resizeTimer);
      self.resizeTimer = window.setTimeout(function () {
        self.relayoutIfNeeded();
      }, RESIZE_DEBOUNCE);
    };

    window.addEventListener("resize", this.onViewportChange);
    window.addEventListener("orientationchange", this.onViewportChange);
    if (window.visualViewport) {
      window.visualViewport.addEventListener("resize", this.onViewportChange);
    }
  };

  Reader.prototype.unbindResize = function () {
    if (!this.onViewportChange) return;
    window.clearTimeout(this.resizeTimer);
    window.removeEventListener("resize", this.onViewportChange);
    window.removeEventListener("orientationchange", this.onViewportChange);
    if (window.visualViewport) {
      window.visualViewport.removeEventListener("resize", this.onViewportChange);
    }
    this.onViewportChange = null;
  };

  Reader.prototype.setStatus = function (message) {
    if (!this.statusEl) return;
    if (message) {
      this.statusEl.textContent = message;
      this.statusEl.hidden = false;
    } else {
      this.statusEl.hidden = true;
    }
  };

  Reader.prototype.showError = function (error) {
    if (window.console && console.error) {
      console.error("Booklet flipbook:", error);
    }
    this.setStatus(
      "A booklet megjelenítése nem sikerült. Töltsd le a PDF-et a fenti gombbal."
    );
  };

  Reader.prototype.loadDocument = function () {
    var self = this;
    return loadPdfJs().then(function (pdfjs) {
      function get(url) {
        return pdfjs.getDocument({
          url: url,
          disableAutoFetch: true,
          disableStream: false
        }).promise;
      }

      return get(self.config.pdf).catch(function (error) {
        if (!self.config.original || self.config.original === self.config.pdf) {
          throw error;
        }
        return get(self.config.original);
      });
    });
  };

  Reader.prototype.buildPages = function () {
    var self = this;

    return this.doc.getPage(1).then(function (firstPage) {
      var viewport = firstPage.getViewport({ scale: 1 });
      self.pageRatio = viewport.width / viewport.height;
      self.createPageElements();
    });
  };

  Reader.prototype.createPageElements = function () {
    this.pages = [];

    for (var i = 1; i <= this.doc.numPages; i++) {
      var pageEl = el("div", "booklet-reader__page");
      pageEl.setAttribute("data-density", "soft");

      var canvas = el("canvas", "booklet-reader__canvas");
      pageEl.appendChild(canvas);

      this.bookEl.appendChild(pageEl);
      this.pages.push({
        number: i,
        el: pageEl,
        canvas: canvas,
        state: "empty",
        task: null
      });
    }
  };

  Reader.prototype.recreateBookDom = function () {
    if (this.bookEl && this.bookEl.parentNode) {
      this.bookEl.remove();
    }

    var newBook = el("div", "booklet-reader__book");
    this.stageEl.insertBefore(newBook, this.statusEl);
    this.bookEl = newBook;
    this.createPageElements();
  };

  // How much room the book may occupy, minus the stage padding.
  Reader.prototype.stageBox = function () {
    var style = window.getComputedStyle(this.stageEl);
    var padX = parseFloat(style.paddingLeft) + parseFloat(style.paddingRight);
    var padY = parseFloat(style.paddingTop) + parseFloat(style.paddingBottom);

    return {
      width: Math.max(this.stageEl.clientWidth - padX, 160),
      height: Math.max(this.stageEl.clientHeight - padY, 160)
    };
  };

  // A single page on narrow screens, a two-page spread when there is room:
  // the page always uses the full width it is allowed to.
  Reader.prototype.computeLayout = function () {
    var box = this.stageBox();
    var ratio = this.pageRatio || 0.707;

    var spread =
      box.width >= SPREAD_MIN_STAGE_WIDTH &&
      box.width / 2 >= SPREAD_MIN_PAGE_WIDTH;

    var width = box.width / (spread ? 2 : 1);
    var height = width / ratio;

    if (height > box.height) {
      height = box.height;
      width = height * ratio;
    }

    return {
      spread: spread,
      width: Math.max(Math.floor(width), 120),
      height: Math.max(Math.floor(height), 160)
    };
  };

  Reader.prototype.initFlip = function (startIndex) {
    var self = this;

    return loadPageFlip().then(function (PageFlip) {
      if (self.destroyed) return;

      var layout = self.computeLayout();

      self.spread = layout.spread;
      self.pageWidth = layout.width;
      self.pageHeight = layout.height;

      // page-flip reads its orientation from the container width, so the
      // container has to state the intended layout explicitly.
      self.bookEl.style.width =
        (layout.spread ? layout.width * 2 : layout.width) + "px";
      self.bookEl.style.height = layout.height + "px";

      self.flip = new PageFlip(self.bookEl, {
        width: layout.width,
        height: layout.height,
        size: "fixed",
        minWidth: layout.width,
        maxWidth: layout.width,
        minHeight: layout.height,
        maxHeight: layout.height,
        autoSize: false,
        usePortrait: !layout.spread,
        showCover: true,
        mobileScrollSupport: false,
        maxShadowOpacity: 0.5,
        drawShadow: !prefersReducedMotion,
        flippingTime: prefersReducedMotion ? 0 : 700,
        startPage: startIndex || 0
      });

      self.flip.loadFromHTML(self.bookEl.querySelectorAll(".booklet-reader__page"));

      self.flip.on("flip", function (event) {
        self.updateWindow(event.data);
      });

      self.updateWindow(self.flip.getCurrentPageIndex());
      return self.renderWindow(self.flip.getCurrentPageIndex());
    });
  };

  Reader.prototype.toggleFullscreen = function () {
    if (document.fullscreenElement === this.frame) {
      document.exitFullscreen();
    } else {
      this.frame.requestFullscreen({ navigationUI: "hide" }).catch(function (error) {
        if (window.console && console.warn) {
          console.warn("Booklet flipbook: fullscreen request failed", error);
        }
      });
    }
  };

  Reader.prototype.handleFullscreenChange = function () {
    var self = this;
    if (this.destroyed) return;

    var isFullscreen = document.fullscreenElement === this.frame;

    if (this.fsBtn) {
      this.fsBtn.setAttribute(
        "aria-label",
        isFullscreen ? "Kilépés a teljes képernyőből" : "Teljes képernyő"
      );
      this.fsBtn.classList.toggle("is-active", isFullscreen);
    }

    // Wait for the browser to settle on the new viewport before measuring.
    requestAnimationFrame(function () {
      requestAnimationFrame(function () {
        self.relayout();
      });
    });
  };

  Reader.prototype.relayoutIfNeeded = function () {
    if (!this.flip || this.destroyed) return;

    var layout = this.computeLayout();
    if (
      layout.spread === this.spread &&
      Math.abs(layout.width - this.pageWidth) < RELAYOUT_THRESHOLD &&
      Math.abs(layout.height - this.pageHeight) < RELAYOUT_THRESHOLD
    ) {
      return;
    }

    this.relayout();
  };

  Reader.prototype.relayout = function () {
    var self = this;
    if (!this.flip || this.destroyed || this.relayouting) return;

    this.relayouting = true;

    var currentIndex = this.flip.getCurrentPageIndex();

    for (var i = 0; i < this.pages.length; i++) {
      if (this.pages[i].task) this.pages[i].task.cancel();
    }

    this.flip.destroy();
    this.flip = null;

    this.recreateBookDom();
    this.initFlip(currentIndex)
      .then(function () {
        self.relayouting = false;
      })
      .catch(function (error) {
        self.relayouting = false;
        if (!self.destroyed) self.showError(error);
      });
  };

  Reader.prototype.updateWindow = function (index) {
    this.updateCounter(index);
    this.renderWindow(index);
  };

  Reader.prototype.updateCounter = function (index) {
    if (!this.counterEl || !this.doc) return;
    this.counterEl.textContent = index + 1 + " / " + this.doc.numPages;
  };

  Reader.prototype.renderWindow = function (index) {
    var jobs = [];

    for (var i = 0; i < this.pages.length; i++) {
      var distance = Math.abs(i - index);
      if (distance <= RENDER_AHEAD) {
        jobs.push(this.renderPage(this.pages[i]));
      } else if (distance > KEEP_AHEAD) {
        this.releasePage(this.pages[i]);
      }
    }

    return Promise.all(jobs);
  };

  Reader.prototype.renderPage = function (page) {
    var self = this;

    if (page.state === "done" || page.state === "rendering") {
      return Promise.resolve();
    }

    var width = page.el.clientWidth || this.pageWidth;
    var height = page.el.clientHeight || this.pageHeight;
    if (!width || !height) return Promise.resolve();

    page.state = "rendering";

    return this.doc
      .getPage(page.number)
      .then(function (pdfPage) {
        if (self.destroyed) return;

        var dpr = window.devicePixelRatio || 1;
        var targetWidth = Math.min(width * dpr, MAX_CANVAS_WIDTH);

        var base = pdfPage.getViewport({ scale: 1 });
        var viewport = pdfPage.getViewport({ scale: targetWidth / base.width });

        var canvas = page.canvas;
        canvas.width = Math.round(viewport.width);
        canvas.height = Math.round(viewport.height);

        var task = pdfPage.render({
          canvasContext: canvas.getContext("2d"),
          viewport: viewport
        });
        page.task = task;

        return task.promise.then(function () {
          page.state = "done";
          page.task = null;
        });
      })
      .catch(function (error) {
        page.state = "empty";
        page.task = null;
        if (error && error.name !== "RenderingCancelledException") {
          if (window.console && console.warn) {
            console.warn("Booklet page " + page.number + ":", error);
          }
        }
      });
  };

  Reader.prototype.releasePage = function (page) {
    if (page.state !== "done") return;
    page.canvas.width = 0;
    page.canvas.height = 0;
    page.state = "empty";
  };

  Reader.prototype.close = function () {
    this.destroyed = true;

    this.unbindResize();
    this.unlockPageScroll();

    if (this.onFullscreenChange) {
      document.removeEventListener("fullscreenchange", this.onFullscreenChange);
      this.onFullscreenChange = null;
    }

    if (document.fullscreenElement === this.frame) {
      document.exitFullscreen();
    }

    for (var i = 0; i < this.pages.length; i++) {
      if (this.pages[i].task) {
        this.pages[i].task.cancel();
      }
    }

    if (this.flip) {
      this.flip.destroy();
      this.flip = null;
    }

    if (this.doc) {
      this.doc.destroy();
      this.doc = null;
    }

    if (this.dialog) {
      this.dialog.close();
      this.dialog.remove();
      this.dialog = null;
    }
  };

  function init(block) {
    var button = block.querySelector(".booklet-flipbook__open");
    if (!button) return;

    var pdf = block.getAttribute("data-booklet-pdf");
    if (!pdf) return;

    var config = {
      pdf: asset(pdf.replace(/([^/]+)$/, "web/$1")),
      original: asset(pdf),
      title: block.getAttribute("data-booklet-title")
    };

    button.addEventListener("click", function () {
      new Reader(config).open();
    });
  }

  function initAll() {
    var blocks = document.querySelectorAll(".booklet-flipbook");
    for (var i = 0; i < blocks.length; i++) {
      init(blocks[i]);
    }
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", initAll);
  } else {
    initAll();
  }
})();
