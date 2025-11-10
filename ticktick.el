;;; ticktick.el --- Sync Org Mode tasks with TickTick -*- lexical-binding: t; -*-

;; Author: Paul Huang
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (request "0.3.0") (simple-httpd "1.5.0"))
;; Keywords: tools, ticktick, org, tasks, todo
;; URL: https://github.com/polhuang/ticktick.el

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; 
;; ticktick.el provides two-way synchronization between TickTick
;; (a popular task management service) and Emacs Org Mode.
;;
;; FEATURES:
;; 
;; - Bidirectional sync: changes in either TickTick or Org Mode are reflected
;;   in both systems
;; - OAuth2 authentication with automatic token refresh
;; - Preserves task metadata: priorities, due dates, completion status
;; - Project-based organization matching TickTick's structure
;; - Optional automatic syncing on focus changes
;;
;; SETUP:
;;
;; 1. Register a TickTick OAuth application at:
;;    https://developer.ticktick.com/
;;
;; 2. Configure your credentials:
;;    (setq ticktick-client-id "your-client-id")
;;    (setq ticktick-client-secret "your-client-secret")
;;
;; 3. Authorize the application:
;;    M-x ticktick-authorize
;;
;; 4. Perform initial sync:
;;    M-x ticktick-sync
;;
;; USAGE:
;;
;; Main commands:
;; - `ticktick-sync': Full bidirectional sync
;; - `ticktick-fetch-to-org': Pull tasks from TickTick to Org
;; - `ticktick-push-from-org': Push Org tasks to TickTick
;; - `ticktick-authorize': Set up OAuth authentication
;; - `ticktick-refresh-token': Manually refresh auth token
;; - `ticktick-toggle-sync-timer': Toggle automatic timer-based syncing
;;
;; Tasks are stored in the file specified by `ticktick-sync-file'
;; (defaults to ~/.emacs.d/ticktick/ticktick.org) with this structure:
;;
;; * Project Name
;; :PROPERTIES:
;; :TICKTICK_PROJECT_ID: abc123
;; :END:
;; ** TODO Task Title [#A]
;; DEADLINE: <2024-01-15 Mon>
;; :PROPERTIES:
;; :TICKTICK_ID: def456
;; :TICKTICK_ETAG: xyz789
;; :SYNC_CACHE: hash
;; :LAST_SYNCED: 2025-01-15T10:30:00+0000
;; :END:
;; Task description content here.
;;
;; CUSTOMIZATION:
;;
;; Key variables you can customize:
;; - `ticktick-sync-file': Path to the org file for tasks
;; - `ticktick-dir': Directory for storing tokens and data
;; - `ticktick-autosync': Enable automatic syncing on focus changes
;; - `ticktick-sync-interval': Enable automatic syncing every N minutes
;; - `ticktick-httpd-port': Port for OAuth callback server
;;
;; For debugging OAuth issues:
;; M-x ticktick-debug-oauth

;;; Code:

(require 'request)
(require 'json)
(require 'url)
(require 'org)
(require 'org-element)
(require 'subr-x)
(require 'simple-httpd)
(require 'cl-lib)

(defgroup ticktick nil
  "Interface with TickTick API."
  :prefix "ticktick-"
  :group 'applications)

(defcustom ticktick-client-id ""
  "TickTick client ID."
  :type 'string
  :group 'ticktick)

(defcustom ticktick-client-secret ""
  "TickTick client secret."
  :type 'string
  :group 'ticktick)

(defcustom ticktick-auth-scopes "tasks:write tasks:read"
  "Space-separated scopes for TickTick API access."
  :type 'string
  :group 'ticktick)


(defcustom ticktick-dir
  (concat user-emacs-directory "ticktick/")
  "Folder in which to save token."
  :group 'ticktick
  :type 'string)

(defcustom ticktick-token-file
  (expand-file-name ".ticktick-token" ticktick-dir)
  "File in which to store TickTick OAuth token."
  :type 'file
  :group 'ticktick)

(defcustom ticktick-sync-file (expand-file-name "ticktick.org" ticktick-dir)
  "Path to the org file where all TickTick tasks will be synchronized."
  :type 'file
  :group 'ticktick)


(defcustom ticktick-autosync nil
  "If non-nil, automatically sync when switching buffers or losing focus."
  :type 'boolean
  :group 'ticktick)

(defcustom ticktick-httpd-port 8080
  "Local port for the OAuth callback server."
  :type 'integer
  :group 'ticktick)

(defcustom ticktick-redirect-uri "http://localhost:8080/ticktick-callback"
  "Redirect URI registered with TickTick. Must match OAuth app settings."
  :type 'string
  :group 'ticktick)

(defcustom ticktick-sync-interval nil
  "Interval in minutes for automatic syncing. If nil, timer-based sync is disabled.
When set to a positive number, TickTick will sync automatically every N minutes.
After changing this value, call `ticktick-toggle-sync-timer' to apply changes."
  :type '(choice (const :tag "Disabled" nil)
                 (integer :tag "Minutes"))
  :group 'ticktick)

(defvar ticktick-token nil
  "Access token plist for accessing the TickTick API.")

(defvar ticktick-oauth-state nil
  "CSRF-prevention state for the OAuth flow.")

(defvar ticktick--sync-timer nil
  "Timer object for periodic syncing.")

;; --- Token persistence helpers -----------------------------------------------
;; 令牌持久化辅助函数：用于保存和加载 OAuth 令牌

(defun ticktick--ensure-dir ()
  "确保 `ticktick-dir` 目录存在，不存在则创建。
参数 t 表示递归创建父目录。"
  (unless (file-directory-p ticktick-dir)
    (make-directory ticktick-dir t)))

(defun ticktick--save-token ()
  "将 `ticktick-token` 持久化保存到 `ticktick-token-file`。
使用 prin1-to-string 将 plist 序列化为可读的字符串格式。"
  (when ticktick-token
    (ticktick--ensure-dir)
    (with-temp-file ticktick-token-file
      (insert (prin1-to-string ticktick-token)))))

(defun ticktick--load-token ()
  "从 `ticktick-token-file` 加载令牌到 `ticktick-token` 变量。
使用 read 函数反序列化文件内容为 plist 结构。"
  (when (file-exists-p ticktick-token-file)
    (with-temp-buffer
      (insert-file-contents ticktick-token-file)
      (goto-char (point-min))
      (setq ticktick-token (read (current-buffer))))))

;; Load any existing token at startup
(ticktick--load-token)

;;; Authorization functions ----------------------------------------------------
;; OAuth 授权相关函数

(defun ticktick--authorization-header ()
  "创建 HTTP Basic 认证头。
格式为 'Basic base64(client_id:client_secret)'，用于令牌请求。"
  (concat "Basic "
          (base64-encode-string
           (concat ticktick-client-id ":" ticktick-client-secret) t)))

(defun ticktick--make-token-request (form-params)
  "向令牌端点发送 POST 请求。
FORM-PARAMS: 表单参数的 alist，例如 grant_type、code 等。
返回值: 包含 access_token、refresh_token 等的 plist，失败返回 nil。

【适配点】此函数中的 URL 需要改为滴答清单国内版的令牌端点。"
  (let* ((form-data
          ;; 将参数列表转换为 URL 编码的表单数据字符串
          (mapconcat
           (lambda (kv)
             (format "%s=%s"
                     (url-hexify-string (car kv))
                     (url-hexify-string (format "%s" (cdr kv)))))
           form-params
           "&"))
         (authorization (ticktick--authorization-header))
         (response-data nil))
    ;; 【适配点】修改此 URL 为滴答清单国内版
    (request "https://ticktick.com/oauth/token"
      :type "POST"
      :headers `(("Authorization" . ,authorization)
                 ("Content-Type" . "application/x-www-form-urlencoded"))
      :data form-data
      :parser (lambda ()
                (let ((json-object-type 'plist)
                      (json-array-type 'list))
                  (json-read)))
      :sync t  ; 同步请求，阻塞直到完成
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (setq response-data data)))
      :error (cl-function
              (lambda (&key response error-thrown &allow-other-keys)
                (message "Token request failed: %s (HTTP %s)"
                         (or error-thrown "unknown error")
                         (and response (request-response-status-code response)))
                (setq response-data nil))))
    ;; 如果成功获取令牌，添加创建时间戳用于过期检查
    (when (and response-data (plist-get response-data :access_token))
      (setq response-data
            (plist-put response-data :created_at (float-time))))
    response-data))


(defun ticktick--exchange-code-for-token (code)
  "使用授权码 CODE 交换访问令牌。
这是 OAuth 授权码流程的第二步，用授权码换取 access_token 和 refresh_token。"
  (ticktick--make-token-request
   `(("grant_type" . "authorization_code")
     ("code" . ,code)
     ("redirect_uri" . ,ticktick-redirect-uri)
     ("scope" . ,ticktick-auth-scopes))))

(defun ticktick--start-callback-server ()
  "启动本地 OAuth 回调服务器（如果尚未运行）。
使用 simple-httpd 包在指定端口启动 HTTP 服务器，用于接收 OAuth 重定向。"
  (setq httpd-port ticktick-httpd-port)
  (unless (httpd-running-p)
    (httpd-start)))

;; servlet 名称决定路径: /ticktick-callback
(defservlet ticktick-callback text/plain (_path query)
  "处理 TickTick OAuth 重定向回调。
验证 state 参数（防止 CSRF 攻击），然后用 code 交换令牌。
query: URL 查询参数，包含 code 和 state。"
  (let ((code  (cadr (assoc "code" query)))
        (state (cadr (assoc "state" query))))
    (cond
     ;; 检查必需参数是否存在
     ((not (and code state))
      (insert "Authentication failed: missing code/state."))
     ;; 验证 state 参数，防止 CSRF 攻击
     ((and ticktick-oauth-state (not (string= state ticktick-oauth-state)))
      (insert "Authentication failed: invalid state."))
     ;; 参数验证通过，交换令牌
     (t
      (let ((tok (ticktick--exchange-code-for-token code)))
        (if tok
            (progn
              (setq ticktick-token tok)
              (ticktick--save-token)
              (insert "Authentication successful! You can close this window.")
              (message "TickTick: authenticated successfully."))
          (insert "Authentication failed while exchanging code.")
          (message "TickTick: token exchange failed.")))))))

(defun ticktick-debug-oauth ()
  "调试 OAuth 配置和连接状态。
显示客户端配置、令牌状态、服务器状态等诊断信息。"
  (interactive)
  (message "=== TickTick OAuth Debug Info ===")
  (message "Client ID: %s" (if (string-empty-p ticktick-client-id) "NOT SET" "SET"))
  (message "Client Secret: %s" (if (string-empty-p ticktick-client-secret) "NOT SET" "SET"))
  (message "Redirect URI: %s" ticktick-redirect-uri)
  (message "Auth Scopes: %s" ticktick-auth-scopes)
  (message "Token file: %s" ticktick-token-file)
  (message "Token exists: %s" (if (and ticktick-token (plist-get ticktick-token :access_token)) "YES" "NO"))
  (when ticktick-token
    (message "Token expires: %s" (if (ticktick-token-expired-p ticktick-token) "EXPIRED" "VALID")))
  (message "Server running: %s" (if (httpd-running-p) "YES" "NO"))
  (message "Server port: %d" ticktick-httpd-port))

;;;###autoload
(defun ticktick-authorize ()
  "启动 OAuth 授权流程，通过本地回调服务器获取访问令牌。
流程：
1. 检查客户端 ID 和密钥是否已设置
2. 启动本地回调服务器
3. 生成随机 state（防止 CSRF）
4. 在浏览器中打开授权 URL
5. 用户授权后，浏览器重定向回本地服务器
6. 回调处理器自动完成令牌交换

【适配点】此函数中的授权 URL 需要改为滴答清单国内版。"
  (interactive)
  (unless (and (stringp ticktick-client-id) (not (string-empty-p ticktick-client-id))
               (stringp ticktick-client-secret) (not (string-empty-p ticktick-client-secret)))
    (user-error "Ticktick-client-id and ticktick-client-secret must be set"))
  (ticktick--start-callback-server)
  ;; 生成 6 位十六进制随机 state
  (setq ticktick-oauth-state (format "%06x" (random (expt 16 6))))
  ;; 【适配点】修改此 URL 为滴答清单国内版
  (let* ((auth-url (concat "https://ticktick.com/oauth/authorize?"
                           (url-build-query-string
                            `(("client_id" ,ticktick-client-id)
                              ("response_type" "code")
                              ("redirect_uri" ,ticktick-redirect-uri)
                              ("scope" ,ticktick-auth-scopes)
                              ("state" ,ticktick-oauth-state))))))
    (browse-url auth-url)
    (message "TickTick: opened browser for OAuth. Waiting for http://localhost:%d/ticktick-callback ..." ticktick-httpd-port)))

;;; Manual fallback (optional): 手动输入授权码的备用方案
(defun ticktick-authorize-manual (code)
  "手动使用授权码 CODE 交换访问令牌（备用方案）。
当自动回调失败时，可以从浏览器地址栏复制 code 参数手动完成认证。"
  (interactive "sPaste ?code= value from redirect URL: ")
  (let ((tok (ticktick--exchange-code-for-token code)))
    (if tok
        (progn
          (setq ticktick-token tok)
          (ticktick--save-token)
          (message "Authorization successful!"))
      (user-error "Failed to obtain access token"))))

;;; Token maintenance ----------------------------------------------------------
;; 令牌维护：刷新和有效性检查

;;;###autoload
(defun ticktick-refresh-token ()
  "使用 refresh_token 刷新 OAuth2 访问令牌。
当访问令牌过期时自动调用，也可手动调用以确保令牌最新。"
  (interactive)
  (when ticktick-token
    (let* ((refresh-token (plist-get ticktick-token :refresh_token))
           (response-data (ticktick--make-token-request
                          `(("grant_type" . "refresh_token")
                            ("refresh_token" . ,refresh-token)
                            ("redirect_uri" . ,ticktick-redirect-uri)
                            ("scope" . ,ticktick-auth-scopes)))))
      (if response-data
          (progn
            (setq ticktick-token response-data)
            (ticktick--save-token)
            (message "Token refreshed!"))
        (message "Failed to refresh token.")))))

(defun ticktick-token-expired-p (token)
  "检查令牌 TOKEN 是否已过期。
使用创建时间 + 有效期 - 30 秒作为过期阈值，提前 30 秒预判过期。
返回值: t 表示已过期，nil 表示仍有效。"
  (let ((expires-in (plist-get token :expires_in))
        (created-at (plist-get token :created_at)))
    (if (and expires-in created-at)
        ;; 当前时间 > (创建时间 + 有效期 - 30秒缓冲)
        (> (float-time) (+ created-at expires-in -30))
      nil)))

(defun ticktick-ensure-token ()
  "确保拥有有效的访问令牌。
检查令牌是否存在、是否包含 access_token、是否过期，必要时自动刷新。"
  (ticktick--load-token)
  (unless (and ticktick-token
               (plist-get ticktick-token :access_token)
               (not (ticktick-token-expired-p ticktick-token)))
    (ticktick-refresh-token)))

;;; Core request ---------------------------------------------------------------
;; 核心 API 请求函数

(defun ticktick--parse-json-maybe ()
  "尝试将当前缓冲区解析为 JSON。
如果缓冲区为空或解析失败，返回 nil。
JSON 对象解析为 plist，数组解析为 list。"
  (goto-char (point-min))
  (let ((json-object-type 'plist)
        (json-array-type 'list)
        (json-false :false))
    (if (zerop (buffer-size))
        nil
      (condition-case _ (json-read)
        (json-error nil)))))

(defun ticktick-request (method endpoint &optional data)
  "向 TickTick API 发送 HTTP 请求。
METHOD: HTTP 方法（GET、POST、DELETE 等）
ENDPOINT: API 端点路径，例如 '/open/v1/project'
DATA: 可选的请求体数据（plist 或 alist，会被转换为 JSON）
返回值: 响应数据的 plist，失败返回 nil

特性：
- 自动确保令牌有效
- 遇到 401 错误自动刷新令牌并重试
- 同步请求，阻塞直到完成

【适配点】此函数中的基础 URL 需要改为滴答清单国内版 API 端点。"
  (ticktick-ensure-token)
  ;; 【适配点】修改此基础 URL 为滴答清单国内版
  (let* ((url (concat "https://api.ticktick.com" endpoint))
         (access-token (plist-get ticktick-token :access_token))
         (headers `(("Authorization" . ,(concat "Bearer " access-token))
                    ("Content-Type" . "application/json")))
         (json-data (and data (json-encode data)))
         (response-data nil))
    (request url
      :type method
      :headers headers
      :data json-data
      :parser #'ticktick--parse-json-maybe
      :sync t  ; 同步请求
      :success (cl-function
                (lambda (&key data response &allow-other-keys)
                  (let ((status (request-response-status-code response)))
                    (cond
                     ;; 2xx 成功响应
                     ((and (>= status 200) (< status 300))
                      (setq response-data data))
                     ;; 401 未授权，刷新令牌后重试
                     ((= status 401)
                      (ticktick-refresh-token)
                      (setq response-data (ticktick-request method endpoint data)))
                     (t (error "HTTP Error %s" status))))))
      :error (cl-function
              (lambda (&key response error-thrown &allow-other-keys)
                (let ((status (and response (request-response-status-code response))))
                  (cond
                   ;; 401 错误处理
                   ((= status 401)
                    (ticktick-refresh-token)
                    (setq response-data (ticktick-request method endpoint data)))
                   (t
                    (message "Request failed: %s"
                             (or (and response (request-response-data response))
                                 error-thrown))
                    (setq response-data nil)))))))
    response-data))

;;; Org conversion helpers -----------------------------------------------------
;; Org Mode 与 TickTick 任务格式转换函数

(defun ticktick--task-to-heading (task)
  "将 TickTick 任务 TASK（plist）转换为 Org Mode 标题字符串。
映射规则：
- status: 2 → DONE, 其他 → TODO
- priority: 5 → [#A], 3 → [#B], 1 → [#C], 0 → 无优先级
- dueDate → DEADLINE
- id → :TICKTICK_ID: 属性
- etag → :TICKTICK_ETAG: 属性（用于检测远程更新）
- content → 任务正文"
  (let ((id (plist-get task :id))
        (title (plist-get task :title))
        (status (plist-get task :status))
        (priority (plist-get task :priority))
        (due (plist-get task :dueDate))
        (etag (plist-get task :etag))
        (content (plist-get task :content)))
    (string-join
     (delq nil
           (list
            ;; 二级标题，包含状态、优先级、标题
            (format "** %s%s %s"
                    (if (= status 2) "DONE" "TODO")
                    (pcase priority (5 " [#A]") (3 " [#B]") (1 " [#C]") (_ ""))
                    title)
            ;; 截止日期
            (when due
              (format "DEADLINE: <%s>" (format-time-string "%F %a" (date-to-time due))))
            ;; 属性抽屉
            ":PROPERTIES:"
            (format ":TICKTICK_ID: %s" id)
            (format ":TICKTICK_ETAG: %s" (or etag ""))
            ":END:"
            ;; 任务内容
            (when content (string-trim content))))
     "\n")))

(defun ticktick--heading-to-task ()
  "将当前位置的 Org 标题转换为 TickTick 任务（alist）。
映射规则（与 task-to-heading 相反）：
- DONE → status=2, TODO → status=0
- [#A] → priority=5, [#B] → priority=3, [#C] → priority=1
- DEADLINE → dueDate (ISO 8601 格式)
- TICKTICK_ID 属性 → id
- 正文内容 → content（跳过 PROPERTIES 抽屉）"
  (let* ((el (org-element-at-point))
         (title (org-element-property :title el))
         (todo (org-element-property :todo-type el))
         (priority (org-element-property :priority el))
         (deadline (org-element-property :deadline el))
         (id (org-entry-get nil "TICKTICK_ID"))
         (content
          (save-excursion
            (save-restriction
              ;; 缩窄到当前子树
              (org-narrow-to-subtree)
              (goto-char (point-min))
              (forward-line)
              ;; 跳过 DEADLINE/SCHEDULED 等规划行
              (while (looking-at org-planning-line-re)
                (forward-line))
              ;; 跳过 PROPERTIES 抽屉
              (when (looking-at ":PROPERTIES:")
                (re-search-forward "^:END:" nil t)
                (forward-line))
              ;; 提取剩余内容作为任务正文
              (string-trim (buffer-substring-no-properties (point) (point-max)))))))
    ;; 返回 alist 格式（JSON 编码需要）
    `(("id" . ,id)
      ("title" . ,title)
      ("status" . ,(if (eq todo 'done) 2 0))
      ("priority" . ,(pcase priority (?A 5) (?B 3) (?C 1) (_ 0)))
      ("dueDate" . ,(when deadline
                      (format-time-string "%FT%T+0000"
                                          (org-timestamp-to-time deadline))))
      ("content" . ,content))))

(defun ticktick--subtree-body-for-hash ()
  "返回当前子树的稳定字符串表示，移除易变属性用于生成哈希。
移除以下内容以确保哈希稳定：
- 整个 PROPERTIES 抽屉
- LAST_SYNCED, SYNC_CACHE, TICKTICK_ETAG, TICKTICK_ID 等属性
目的：只有任务实际内容变化时，哈希才会改变。"
  (let* ((raw (buffer-substring-no-properties
               (org-entry-beginning-position) (org-entry-end-position))))
    (with-temp-buffer
      (insert raw)
      (goto-char (point-min))
      ;; 移除整个属性抽屉
      (while (re-search-forward "^:PROPERTIES:\n\\(?:.*\n\\)*?:END:\n?" nil t)
        (replace-match "" nil nil))
      ;; 移除任何游离的易变属性行（如果不在抽屉中）
      (goto-char (point-min))
      (while (re-search-forward
              "^:\\(LAST_SYNCED\\|SYNC_CACHE\\|TICKTICK_ETAG\\|TICKTICK_ID\\):.*\n" nil t)
        (replace-match "" nil nil))
      (buffer-string))))

(defun ticktick--should-sync-p ()
  "判断当前子树自上次同步后是否发生变化。
逻辑：
- 没有 etag → 需要同步（可能是新任务）
- 计算当前内容哈希 → 与 SYNC_CACHE 比较 → 不同则需要同步
返回值: 非 nil 表示需要同步，nil 表示无需同步。"
  (let* ((etag   (org-entry-get nil "TICKTICK_ETAG"))
         (cached (org-entry-get nil "SYNC_CACHE"))
         (digest (secure-hash 'sha1 (ticktick--subtree-body-for-hash))))
    (or (not etag) (not (and cached (string= cached digest))))))

(defun ticktick--update-sync-meta ()
  "更新当前子树的同步元数据。
设置：
- LAST_SYNCED: 当前时间戳
- SYNC_CACHE: 内容哈希值
用途：记录同步状态，避免重复推送未改变的任务。"
  (let* ((digest (secure-hash 'sha1 (ticktick--subtree-body-for-hash))))
    (org-set-property "LAST_SYNCED" (format-time-string "%FT%T%z"))
    (org-set-property "SYNC_CACHE"  digest)))

;;; Sync functions -------------------------------------------------------------
;; 同步核心函数：项目和任务的双向同步

(defun ticktick--create-project-heading (project-title project-id)
  "为项目 PROJECT-TITLE 创建新的 Org 一级标题。
PROJECT-ID: TickTick 项目 ID
返回值: 新创建标题的起始位置

生成格式：
* 项目名称
:PROPERTIES:
:TICKTICK_PROJECT_ID: xxx
:END:"
  (goto-char (point-max))
  (unless (bolp) (insert "\n"))            ; 确保从新行开始
  (let ((start (point)))                   ; 记录标题起始位置
    (insert (format "* %s\n:PROPERTIES:\n:TICKTICK_PROJECT_ID: %s\n:END:\n"
                    project-title project-id))
    start))


(defun ticktick--sync-task (task project-pos)
  "将单个任务 TASK 同步到项目 PROJECT-POS 下。
TASK: TickTick 任务 plist
PROJECT-POS: 项目标题的位置

逻辑：
1. 通过 TICKTICK_ID 查找任务是否已存在
2. 存在且 etag 不同 → 更新任务（删除旧的，插入新的）
3. 不存在 → 插入新任务
4. 更新同步元数据"
  (let* ((id (plist-get task :id))
         (etag (plist-get task :etag))
         (existing-pos (ticktick--find-task-under-project project-pos id)))
    (if existing-pos
        ;; 任务已存在，检查是否需要更新
        (save-excursion
          (goto-char existing-pos)
          (let ((existing-etag (org-entry-get nil "TICKTICK_ETAG")))
            ;; etag 不同说明远程有更新
            (unless (string= existing-etag etag)
              (delete-region (org-entry-beginning-position)
                             (org-entry-end-position))
              (insert (ticktick--task-to-heading task))
              (ticktick--update-sync-meta))))
      ;; 任务不存在，创建新任务
      (save-excursion
        (goto-char project-pos)
        (outline-next-heading)
        (insert (ticktick--task-to-heading task) "\n")
        (ticktick--update-sync-meta)))))

(defun ticktick--sync-project (project)
  "同步单个项目 PROJECT 及其所有任务。
PROJECT: 项目 plist，包含 :id 和 :name

流程：
1. 查找或创建项目标题
2. 获取项目的所有任务数据（API 调用）
3. 遍历任务，逐个同步"
  (let* ((project-id (plist-get project :id))
         (project-title (plist-get project :name))
         (project-heading-re (format "^\\* %s$" (regexp-quote project-title)))
         (project-pos (save-excursion
                        (goto-char (point-min))
                        (when (re-search-forward project-heading-re nil t)
                          (match-beginning 0)))))
    ;; 如果项目标题不存在，创建它
    (unless project-pos
      (setq project-pos (ticktick--create-project-heading project-title project-id)))
    (goto-char project-pos)
    (outline-show-subtree)
    ;; 获取项目数据，包含所有任务
    (let* ((project-data (ticktick-request "GET" (format "/open/v1/project/%s/data" project-id)))
           (tasks (plist-get project-data :tasks)))
      ;; 遍历任务，逐个同步
      (dolist (task tasks)
        (ticktick--sync-task task project-pos)))))

(defun ticktick--update-task (task project-id id)
  "更新 TickTick 中已存在的任务。
TASK: 任务数据 alist
PROJECT-ID: 所属项目 ID
ID: 任务 ID
调用 API: POST /open/v1/task/{id}"
  (ticktick-request "POST" (format "/open/v1/task/%s" id)
                    (append task `(("projectId" . ,project-id))))
  (ticktick--update-sync-meta)
  (message "Updated: %s" (alist-get "title" task)))

(defun ticktick--create-task (task project-id)
  "在 TickTick 中创建新任务。
TASK: 任务数据 alist
PROJECT-ID: 所属项目 ID
调用 API: POST /open/v1/task

成功后：
- 设置 TICKTICK_ID 属性
- 设置 TICKTICK_ETAG 属性
- 更新同步元数据"
  (let ((resp (ticktick-request "POST" "/open/v1/task"
                                (append task `(("projectId" . ,project-id))))))
    (when resp
      (org-set-property "TICKTICK_ID" (plist-get resp :id))
      (org-set-property "TICKTICK_ETAG" (plist-get resp :etag))
      (ticktick--update-sync-meta)
      (message "Created: %s" (plist-get resp :title)))))

(defun ticktick--find-task-under-project (project-heading id)
  "在项目 PROJECT-HEADING 下查找任务 ID 的位置。
使用 org-map-entries 遍历子树，通过 TICKTICK_ID 属性匹配。
返回值: 任务位置或 nil（未找到）"
  (save-excursion
    (goto-char project-heading)
    (catch 'found
      (org-map-entries
       (lambda ()
         (when (string= (org-entry-get nil "TICKTICK_ID") id)
           (throw 'found (point))))
       nil 'tree)  ; 'tree 表示只遍历当前子树
      nil)))

(defun ticktick-fetch-to-org ()
  "从 TickTick 拉取所有任务到 Org 文件（不重复创建）。
这是单向同步：TickTick → Org

流程：
1. 获取所有项目列表（包括 Inbox）
2. 对每个项目调用 ticktick--sync-project
3. 保存 org 文件

【交互式命令】可通过 M-x ticktick-fetch-to-org 调用"
  (interactive)
  (let* ((inbox-project `(:id "inbox" :name "Inbox"))
         ;; 获取所有项目
         (projects (ticktick-request "GET" "/open/v1/project"))
         ;; 将 Inbox 添加到项目列表
         (all-projects (cons inbox-project projects)))
    (with-current-buffer (find-file-noselect ticktick-sync-file)
      (org-with-wide-buffer
       ;; 遍历所有项目，逐个同步
       (dolist (project all-projects)
         (ticktick--sync-project project))
       (save-buffer)))))

(defun ticktick-push-from-org ()
  "将 Org 文件中的所有更新推送到 TickTick。
这是单向同步：Org → TickTick

流程：
1. 遍历 org 文件中的所有二级标题（任务）
2. 检查任务是否需要同步（ticktick--should-sync-p）
3. 有 TICKTICK_ID → 更新任务
4. 无 TICKTICK_ID → 创建新任务
5. 保存 org 文件

【交互式命令】可通过 M-x ticktick-push-from-org 调用"
  (interactive)
  (with-current-buffer (find-file-noselect ticktick-sync-file)
    (org-with-wide-buffer
     (goto-char (point-min))
     ;; 遍历所有标题
     (while (outline-next-heading)
       ;; 只处理二级标题（任务），排除一级标题（项目）
       (when (and (= (org-current-level) 2)
                  (not (org-entry-get nil "TICKTICK_PROJECT_ID")))
         ;; 检查是否需要同步（内容是否改变）
         (when (ticktick--should-sync-p)
           (let* ((task (ticktick--heading-to-task))
                  ;; 继承父级的项目 ID
                  (project-id (org-entry-get nil "TICKTICK_PROJECT_ID" t))
                  (id (org-entry-get nil "TICKTICK_ID")))
             ;; 根据是否有 ID 决定更新还是创建
             (if (and id (not (string-empty-p id)))
                 (ticktick--update-task task project-id id)
               (ticktick--create-task task project-id))))))
     (save-buffer))))


(defun ticktick-sync ()
  "完整的双向同步：先推送本地更改，再拉取远程更新。
这是最常用的同步命令，结合了 push 和 fetch。

流程：
1. 推送本地更改到 TickTick (ticktick-push-from-org)
2. 等待 1 秒（让远程更新生效）
3. 从 TickTick 拉取更新 (ticktick-fetch-to-org)

【交互式命令】可通过 M-x ticktick-sync 调用"
  (interactive)
  (ticktick-push-from-org)
  (sit-for 1)  ; 短暂等待，避免时序问题
  (ticktick-fetch-to-org))

(defun ticktick--autosync ()
  "自动同步函数（如果启用）。
检查 ticktick-autosync 变量，忽略所有错误以避免中断用户操作。
通常由失焦事件触发。"
  (when ticktick-autosync
    (when (file-exists-p ticktick-sync-file)
      (ignore-errors (ticktick-sync)))))

(defun ticktick--setup-sync-timer ()
  "根据 `ticktick-sync-interval` 设置或取消定时同步。
如果 interval 有效（正数），创建定时器；否则取消现有定时器。
定时器会定期调用 ticktick--timer-sync。"
  (when ticktick--sync-timer
    (cancel-timer ticktick--sync-timer)
    (setq ticktick--sync-timer nil))
  (when (and ticktick-sync-interval
             (numberp ticktick-sync-interval)
             (> ticktick-sync-interval 0))
    (setq ticktick--sync-timer
          (run-at-time ticktick-sync-interval
                       (* ticktick-sync-interval 60)  ; 分钟转秒
                       #'ticktick--timer-sync))))

(defun ticktick--timer-sync ()
  "定时器调用的同步函数。
与 ticktick--autosync 类似，但专门用于定时触发。
忽略错误，避免定时器崩溃。"
  (when (file-exists-p ticktick-sync-file)
    (ignore-errors (ticktick-sync))))

;;;###autoload
(defun ticktick-toggle-sync-timer ()
  "切换定时自动同步开关。
如果定时器正在运行 → 停止它
如果定时器未运行且 interval 有效 → 启动它
否则 → 提示用户设置 ticktick-sync-interval

【交互式命令】可通过 M-x ticktick-toggle-sync-timer 调用"
  (interactive)
  (if ticktick--sync-timer
      (progn
        (cancel-timer ticktick--sync-timer)
        (setq ticktick--sync-timer nil)
        (message "TickTick timer sync disabled"))
    (if (and ticktick-sync-interval
             (numberp ticktick-sync-interval)
             (> ticktick-sync-interval 0))
        (progn
          (ticktick--setup-sync-timer)
          (message "TickTick timer sync enabled (every %d minutes)" ticktick-sync-interval))
      (message "Set ticktick-sync-interval to enable timer sync"))))


;; 当 Emacs 失去焦点时运行自动同步

(defun ticktick--maybe-autosync-on-focus-change (&rest _)
  "当选定的 frame 失去焦点时触发自动同步。
使用 idle timer 避免阻塞焦点切换事件。
只在新版本 Emacs 中有效（有 frame-focus-state 函数）。"
  (when (and (fboundp 'frame-focus-state)
             (not (frame-focus-state (selected-frame))))
    (run-with-idle-timer 0 nil #'ticktick--autosync)))

;;;###autoload
(defun ticktick-enable-autosync-on-blur ()
  "启用失焦自动同步功能。
当 Emacs 窗口失去焦点时自动执行同步。
兼容新旧版本的 Emacs：
- 新版: 使用 after-focus-change-function
- 旧版: 使用 focus-out-hook（已废弃）

【交互式命令】可通过 M-x ticktick-enable-autosync-on-blur 调用"
  (if (boundp 'after-focus-change-function)
      (add-function :after after-focus-change-function
                    #'ticktick--maybe-autosync-on-focus-change)
    (with-suppressed-warnings ((obsolete focus-out-hook))
      (add-hook 'focus-out-hook #'ticktick--autosync))))

;;;###autoload
(defun ticktick-disable-autosync-on-blur ()
  "禁用失焦自动同步功能。
移除焦点变化时的同步钩子。

【交互式命令】可通过 M-x ticktick-disable-autosync-on-blur 调用"
  (if (boundp 'after-focus-change-function)
      (remove-function after-focus-change-function
                       #'ticktick--maybe-autosync-on-focus-change)
    (with-suppressed-warnings ((obsolete focus-out-hook))
      (remove-hook 'focus-out-hook #'ticktick--autosync))))


(provide 'ticktick)
;;; ticktick.el ends here
