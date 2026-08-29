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

  var MAX_CANVAS_WIDTH = 1400;

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
  }

  Reader.prototype.open = function () {
    var self = this;

    this.buildDialog();
    document.body.appendChild(this.dialog);
    this.dialog.showModal();

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

    var bar = el("div", "booklet-reader__bar");

    var title = el("p", "booklet-reader__title", this.config.title || "Booklet");

    var actions = el("div", "booklet-reader__actions");

    var download = el("a", "booklet-reader__download", "PDF letöltése");
    download.href = this.config.original || this.config.pdf;
    download.setAttribute("download", "");

    var close = el("button", "booklet-reader__close");
    close.type = "button";
    close.setAttribute("aria-label", "Bezárás");
    close.innerHTML = "<span aria-hidden='true'>&times;</span>";
    close.addEventListener("click", function () {
      self.close();
    });

    actions.appendChild(download);
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

    dialog.appendChild(bar);
    dialog.appendChild(stage);
    dialog.appendChild(nav);

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
    this.bookEl = book;
    this.statusEl = status;
    this.counterEl = counter;
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

      for (var i = 1; i <= self.doc.numPages; i++) {
        var pageEl = el("div", "booklet-reader__page");
        pageEl.setAttribute("data-density", "soft");

        var canvas = el("canvas", "booklet-reader__canvas");
        pageEl.appendChild(canvas);

        self.bookEl.appendChild(pageEl);
        self.pages.push({
          number: i,
          el: pageEl,
          canvas: canvas,
          state: "empty",
          task: null
        });
      }
    });
  };

  Reader.prototype.initFlip = function () {
    var self = this;

    return loadPageFlip().then(function (PageFlip) {
      if (self.destroyed) return;

      var stage = self.dialog.querySelector(".booklet-reader__stage");
      var available = stage.getBoundingClientRect();

      var maxH = Math.max(available.height - 16, 200);
      var maxW = Math.max(available.width - 16, 200);
      var pageH = maxH;
      var pageW = pageH * self.pageRatio;

      if (pageW * 2 > maxW) {
        pageW = maxW / 2;
        pageH = pageW / self.pageRatio;
      }

      self.pageWidth = Math.round(pageW);
      self.pageHeight = Math.round(pageH);

      self.flip = new PageFlip(self.bookEl, {
        width: Math.round(pageW),
        height: Math.round(pageH),
        size: "stretch",
        minWidth: 200,
        maxWidth: 1000,
        minHeight: 280,
        maxHeight: 1500,
        showCover: true,
        usePortrait: true,
        mobileScrollSupport: false,
        maxShadowOpacity: 0.5,
        drawShadow: !prefersReducedMotion,
        flippingTime: prefersReducedMotion ? 0 : 700
      });

      self.flip.loadFromHTML(self.bookEl.querySelectorAll(".booklet-reader__page"));

      self.flip.on("flip", function (event) {
        self.updateWindow(event.data);
      });

      self.updateWindow(self.flip.getCurrentPageIndex());
      return self.renderWindow(self.flip.getCurrentPageIndex());
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
